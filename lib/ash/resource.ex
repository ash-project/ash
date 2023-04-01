defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.Dsl`
  """

  @type t :: module
  @type record :: struct()

  use Spark.Dsl,
    single_extension_kinds: [:data_layer],
    many_extension_kinds: [
      :authorizers,
      :notifiers
    ],
    default_extensions: [
      data_layer: Ash.DataLayer.Simple,
      extensions: [Ash.Resource.Dsl]
    ]

  @doc false
  @impl Spark.Dsl
  def init(opts) do
    if opts[:data_layer] == :embedded do
      {:ok,
       opts
       |> Keyword.put(:data_layer, Ash.DataLayer.Simple)
       |> Keyword.put(:embedded?, true)}
    else
      {:ok, opts}
    end
  end

  @doc false
  @impl Spark.Dsl
  def handle_opts(opts) do
    quote bind_quoted: [
            embedded?: opts[:embedded?]
          ] do
      if embedded? do
        @persist {:embedded?, true}

        require Ash.EmbeddableType

        Ash.EmbeddableType.define_embeddable_type()
      else
        use Ash.Type

        @impl true
        def storage_type, do: :map

        @impl Ash.Type
        def cast_input(nil, _), do: {:ok, nil}
        def cast_input(%struct{} = value, _) when struct == __MODULE__, do: {:ok, value}

        @impl Ash.Type
        def cast_stored(nil, _), do: {:ok, nil}

        def cast_stored(_, _),
          do:
            {:error,
             "Cannot cast a non embedded resource from storage. A non-embedded resource may only be used as an argument type."}

        @impl Ash.Type
        def dump_to_native(nil, _), do: {:ok, nil}

        def dump_to_native(_, _),
          do:
            {:error,
             "Cannot dump a non embedded resource to native. A non-embedded resource may only be used as an argument type."}
      end
    end
  end

  @doc false
  # sobelow_skip ["DOS.StringToAtom"]
  @impl Spark.Dsl
  def handle_before_compile(_opts) do
    quote do
      require Ash.Schema

      new_notifiers =
        @persist[:notifiers] ++ Module.get_attribute(__MODULE__, :simple_notifiers) || []

      @persist {:notifiers, new_notifiers}

      Ash.Schema.define_schema()

      @all_arguments __MODULE__
                     |> Ash.Resource.Info.actions()
                     |> Enum.flat_map(& &1.arguments)
                     |> Enum.map(& &1.name)
                     |> Enum.uniq()

      @arguments_by_action __MODULE__
                           |> Ash.Resource.Info.actions()
                           |> Map.new(fn action ->
                             {action.name, Enum.map(action.arguments, & &1.name)}
                           end)

      @all_attributes __MODULE__
                      |> Ash.Resource.Info.public_attributes()
                      |> Enum.map(& &1.name)
                      |> Enum.uniq()

      if AshPolicyAuthorizer.Authorizer in @extensions do
        raise """
        AshPolicyAuthorizer has been deprecated and is now built into Ash core.

        To use it, replace `authorizers: [AshPolicyAuthorizer.Authorizer]` with `authorizers: [Ash.Policy.Authorizer]`
        """
      end

      if api = Ash.Resource.Info.define_interface_for(__MODULE__) do
        if api == __MODULE__ do
          raise "code_interface.define_for should be set to the API module you want it to call, not the resource."
        end

        require Ash.CodeInterface
        Ash.CodeInterface.define_interface(api, __MODULE__)
      end

      @default_short_name __MODULE__
                          |> Module.split()
                          |> List.last()
                          |> Macro.underscore()
                          |> String.to_atom()

      def default_short_name do
        @default_short_name
      end

      @primary_key_with_types __MODULE__
                              |> Ash.Resource.Info.attributes()
                              |> Enum.filter(& &1.primary_key?)
                              |> Enum.map(&{&1.name, &1.type})

      @primary_key @primary_key_with_types |> Enum.map(&elem(&1, 0))

      if Ash.Resource.Info.primary_key_simple_equality?(__MODULE__) do
        def primary_key_matches?(left, right) do
          Map.take(left, @primary_key) == Map.take(right, @primary_key)
        end
      else
        case @primary_key_with_types do
          [{field, type}] ->
            @pkey_field field
            @pkey_type type

            def primary_key_matches?(left, right) do
              Ash.Type.equal?(
                @pkey_type,
                Map.fetch!(left, @pkey_field),
                Map.fetch!(right, @pkey_field)
              )
            end

            def primary_key_matches?(_left, _right), do: false

          _ ->
            def primary_key_matches?(left, right) do
              Enum.all?(@primary_key_with_types, fn {name, type} ->
                Ash.Type.equal?(type, Map.fetch!(left, name), Map.fetch!(right, name))
              end)
            end
        end
      end

      @doc """
      Validates that the keys in the provided input are valid for at least one action on the resource.

      Raises a KeyError error at compile time if not. This exists because generally a struct should only ever
      be created by Ash as a result of a successful action. You should not be creating records manually in code,
      e.g `%MyResource{value: 1, value: 2}`. Generally that is fine, but often with embedded resources it is nice
      to be able to validate the keys that are being provided, e.g

      ```elixir
      Resource
      |> Ash.Changeset.for_create(:create, %{embedded: EmbeddedResource.input(foo: 1, bar: 2)})
      |> MyApp.Api.create()
      ```
      """
      @spec input(values :: map | Keyword.t()) :: map | no_return
      def input(opts) do
        Map.new(opts, fn {key, value} ->
          if key in @all_arguments || key in @all_attributes do
            {key, value}
          else
            raise KeyError, key: key
          end
        end)
      end

      @doc """
      Same as `input/1`, except restricts the keys to values accepted by the action provided.
      """
      @spec input(values :: map | Keyword.t(), action :: atom) :: map | no_return
      def input(opts, action) do
        case Map.fetch(@arguments_by_action, action) do
          :error ->
            raise ArgumentError, message: "No such action #{inspect(action)}"

          {:ok, args} ->
            action = Ash.Resource.Info.action(__MODULE__, action)

            Map.new(opts, fn {key, value} ->
              if key in action.accept do
                {key, value}
              else
                raise KeyError, key: key
              end
            end)
        end
      end
    end
  end

  @spec set_metadata(Ash.Resource.record(), map) :: Ash.Resource.record()
  def set_metadata(record, map) do
    %{record | __metadata__: Ash.Helpers.deep_merge_maps(record.__metadata__, map)}
  end

  @doc false
  def set_meta(%{__meta__: _} = struct, meta) do
    %{struct | __meta__: meta}
  end

  def set_meta(struct, _), do: struct

  @spec put_metadata(Ash.Resource.record(), atom, term) :: Ash.Resource.record()
  def put_metadata(record, key, term) do
    set_metadata(record, %{key => term})
  end

  @doc "Sets a list of loaded key or paths to a key back to their original unloaded stated"
  @spec unload_many(
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page(),
          list(atom) | list(list(atom))
        ) ::
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page()
  def unload_many(data, paths) do
    Enum.reduce(paths, data, &unload(&2, &1))
  end

  @doc "Sets a loaded key or path to a key back to its original unloaded stated"
  @spec unload(
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page(),
          atom | list(atom)
        ) ::
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page()
  def unload(nil, _), do: nil

  def unload(%struct{results: results} = page, path)
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] do
    %{page | results: unload(results, path)}
  end

  def unload(records, path) when is_list(records) do
    Enum.map(records, &unload(&1, path))
  end

  def unload(record, [path]) do
    unload(record, path)
  end

  def unload(record, [key | rest]) do
    Map.update!(record, key, &unload(&1, rest))
  end

  def unload(%struct{} = record, key) when is_atom(key) do
    Map.put(record, key, Map.get(struct.__struct__(), key))
  end

  def unload(other, _), do: other

  @doc "Returns true if the load or path to load has been loaded"
  @spec loaded?(
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page(),
          atom | list(atom)
        ) ::
          boolean
  def loaded?(nil, _), do: true

  def loaded?(%page{results: results}, path) when page in [Ash.Page.Keyset, Ash.Page.Offset] do
    loaded?(results, path)
  end

  def loaded?(records, path) when not is_list(path) do
    loaded?(records, [path])
  end

  def loaded?(records, path) when is_list(records) do
    Enum.all?(records, &loaded?(&1, path))
  end

  def loaded?(%Ash.NotLoaded{}, _), do: false

  def loaded?(_, []), do: true

  def loaded?(record, [key | rest]) do
    record
    |> Map.get(key)
    |> loaded?(rest)
  end

  @spec get_metadata(Ash.Resource.record(), atom | list(atom)) :: term
  def get_metadata(record, key_or_path) do
    get_in(record.__metadata__ || %{}, List.wrap(key_or_path))
  end

  @spec selected?(Ash.Resource.record(), atom) :: boolean
  def selected?(%resource{} = record, field) do
    case get_metadata(record, :selected) do
      nil ->
        !!Ash.Resource.Info.attribute(resource, field)

      select ->
        if field in select do
          true
        else
          attribute = Ash.Resource.Info.attribute(resource, field)

          attribute && attribute.primary_key?
        end
    end
  end

  @doc false
  def reserved_names do
    [
      :__struct__,
      :__meta__,
      :__metadata__,
      :__order__,
      :calculations,
      :aggregates
    ]
  end

  @impl Spark.Dsl
  def explain(dsl_state, _) do
    Ash.Resource.Info.description(dsl_state)
  end
end
