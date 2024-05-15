defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  [Resource DSL documentation](dsl-ash-resource.html)
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
    ],
    extension_kind_types: [
      authorizers: {:wrap_list, {:behaviour, Ash.Authorizer}},
      data_layer: {:behaviour, Ash.DataLayer},
      notifiers: {:wrap_list, {:behaviour, Ash.Notifier}}
    ],
    opt_schema: [
      simple_notifiers: [
        type: {:list, {:behaviour, Ash.Notifier}},
        doc: "Notifiers with no DSL."
      ],
      validate_domain_inclusion?: [
        type: :boolean,
        doc: "Whether or not to validate that this resource is included in a domain.",
        default: true
      ],
      domain: [
        type: {:behaviour, Ash.Domain},
        doc:
          "The domain to use when interacting with this resource. Also sets defaults for various options that ask for a domain."
      ],
      embed_nil_values?: [
        type: :boolean,
        default: true,
        doc:
          "Whether or not to include keys with `nil` values in an embedded representation. Has no effect unless resource is an embedded resource."
      ]
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

  @impl true
  def verify(module, opts) do
    if Application.get_env(:ash, :validate_domain_resource_inclusion?, true) &&
         Keyword.get(opts, :validate_domain_inclusion?, true) &&
         !Ash.Resource.Info.embedded?(module) &&
         Code.ensure_loaded?(Mix.Project) do
      otp_app = Mix.Project.config()[:app]

      domains =
        Application.get_env(otp_app, :ash_domains, [])

      domains =
        if domain = Ash.Resource.Info.domain(module) do
          [domain | domains]
        else
          domains
        end

      contained_in_domain =
        if is_nil(domain) || Ash.Domain.Info.allow_unregistered?(domain) do
          true
        else
          domains
          |> Enum.flat_map(&Ash.Domain.Info.resources/1)
          |> Enum.any?(&(&1 == module))
        end

      if !contained_in_domain do
        IO.warn("""
        Resource #{inspect(module)} is not present in any known Ash.Domain module.

        Domain modules checked: #{inspect(domains)}

        We check the following configuration for domain modules:

           config :#{otp_app}, ash_domains: #{inspect(domains)}

        To resolve this warning, do one of the following.

        1. Add the resource to one of your configured domain modules.
        2. Add the option `validate_domain_inclusion?: false` to `use Ash.Resource`
        3. Configure all resources not to warn, with `config :ash, :validate_domain_resource_inclusion?, false`
        """)
      end
    end
  end

  @doc false
  @impl Spark.Dsl
  def handle_opts(opts) do
    quote bind_quoted: [
            opts: opts,
            embedded?: opts[:embedded?],
            domain: opts[:domain],
            has_domain?: Keyword.has_key?(opts, :domain),
            embed_nil_values?: opts[:embed_nil_values?]
          ] do
      @persist {:simple_notifiers, List.wrap(opts[:simple_notifiers])}

      cond do
        embedded? && has_domain? ->
          raise """
          Configuration Error in #{inspect(__MODULE__)}:

          `domain` option must not be specified for embedded resource.
          """

        embedded? || has_domain? ->
          :ok

        true ->
          IO.warn("""
          Configuration Error:

          `domain` option missing for #{inspect(__MODULE__)}

          If you wish to make a resource compatible with multiple domains, set the domain to `nil` explicitly.

          Example configuration:

          use Ash.Resource, #{String.trim_trailing(String.trim_leading(inspect([{:domain, YourDomain} | opts], pretty: true), "["), "]")}
          """)
      end

      if domain do
        @persist {:domain, domain}
      end

      if embedded? do
        @persist {:embedded?, true}

        require Ash.EmbeddableType

        Ash.EmbeddableType.define_embeddable_type(embed_nil_values?: embed_nil_values?)
      end
    end
  end

  @doc false
  # sobelow_skip ["DOS.StringToAtom"]
  @impl Spark.Dsl
  def handle_before_compile(_opts) do
    quote do
      require Ash.Schema

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

      if Ash.Resource.Info.define_interface?(__MODULE__) do
        if domain =
             Ash.Resource.Info.code_interface_domain(__MODULE__) ||
               Ash.Resource.Info.domain(__MODULE__) do
          if domain == __MODULE__ do
            raise "code_interface.domain should be set to a Domain module, not the resource."
          end

          require Ash.CodeInterface
          Ash.CodeInterface.define_interface(domain, __MODULE__)
        end
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

      if !Enum.empty?(@primary_key) do
        if Ash.Resource.Info.primary_key_simple_equality?(__MODULE__) do
          def primary_key_matches?(left, right) do
            left_taken = Map.take(left, @primary_key)
            left_taken == Map.take(right, @primary_key) && Enum.all?(Map.values(left_taken))
          end
        else
          case @primary_key_with_types do
            [{field, type}] ->
              @pkey_field field
              @pkey_type type

              def primary_key_matches?(left, right) when not is_nil(left) and not is_nil(right) do
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
                  with {:ok, left_value} when not is_nil(left_value) <- Map.fetch(left, name),
                       {:ok, right_value} when not is_nil(right_value) <- Map.fetch(right, name) do
                    Ash.Type.equal?(type, left_value, right_value)
                  else
                    _ ->
                      false
                  end
                end)
              end
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
      |> Ash.create()
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

  @doc """
  Returns true if the load or path to load has been loaded

  ## Options

  - `lists`: set to `:any` to have this return true if any record in a list that appears has the value loaded. Default is `:all`.
  - `unknown`: set to `true` to have unknown paths (like nil values or non-resources) return true. Defaults to `false`
  - `strict?`: set to `true` to return false if a calculation with arguments is being checked
  """
  @spec loaded?(
          nil | list(Ash.Resource.record()) | Ash.Resource.record() | Ash.Page.page(),
          atom | Ash.Query.Calculation.t() | Ash.Query.Aggregate.t() | list(atom),
          opts :: Keyword.t()
        ) ::
          boolean
  def loaded?(data, path, opts \\ [])

  def loaded?(records, path, opts) when not is_list(path) do
    loaded?(records, List.wrap(path), opts)
  end

  def loaded?(%Ash.NotLoaded{}, _, _opts), do: false
  def loaded?(_, [], _opts), do: true
  # We actually just can't tell here, so we say no
  def loaded?(nil, _, opts), do: Keyword.get(opts, :unknown, false)

  def loaded?(%page{results: results}, path, opts)
      when page in [Ash.Page.Keyset, Ash.Page.Offset] do
    loaded?(results, path, opts)
  end

  def loaded?(records, path, opts) when is_list(records) do
    case Keyword.get(opts, :lists, :all) do
      :all ->
        Enum.all?(records, &loaded?(&1, path, opts))

      :any ->
        Enum.any?(records, &loaded?(&1, path, opts))
    end
  end

  def loaded?(%resource{} = record, [%Ash.Query.Calculation{} = calculation | rest], opts) do
    if calculation.calc_name do
      resource_calculation = Ash.Resource.Info.calculation(resource, calculation.calc_name)

      # we can't say for sure if the original arguments provided
      # were the same as these, so this is always false
      if opts[:strict?] && Enum.any?(resource_calculation.arguments) do
        false
      else
        if calculation.load do
          loaded_on_type?(
            Map.get(record, calculation.load),
            rest,
            calculation.type,
            calculation.constraints,
            opts
          )
        else
          case Map.fetch(record.calculations, calculation.name) do
            {:ok, value} ->
              loaded_on_type?(
                value,
                rest,
                calculation.type,
                calculation.constraints,
                opts
              )

            :error ->
              false
          end
        end
      end
    else
      if calculation.load do
        loaded_on_type?(
          Map.get(record, calculation.load),
          rest,
          calculation.type,
          calculation.constraints,
          opts
        )
      else
        case Map.fetch(record.calculations, calculation.name) do
          {:ok, value} ->
            loaded_on_type?(
              value,
              rest,
              calculation.type,
              calculation.constraints,
              opts
            )

          :error ->
            false
        end
      end
    end
  end

  def loaded?(record, [%Ash.Query.Aggregate{} = aggregate | rest], opts) do
    if aggregate.load do
      loaded_on_type?(
        Map.get(record, aggregate.load),
        rest,
        aggregate.type,
        aggregate.constraints,
        opts
      )
    else
      case Map.fetch(record.aggregates, aggregate.name) do
        {:ok, value} ->
          loaded_on_type?(
            value,
            rest,
            aggregate.type,
            aggregate.constraints,
            opts
          )

        _ ->
          false
      end
    end
  end

  def loaded?(record, [%Ash.Resource.Aggregate{} = aggregate | rest], opts) do
    loaded_on_type?(
      Map.get(record, aggregate.name),
      rest,
      aggregate.type,
      aggregate.constraints,
      opts
    )
  end

  def loaded?(record, [%Ash.Resource.Calculation{} = resource_calculation | rest], opts) do
    if opts[:strict?] && Enum.any?(resource_calculation.arguments) do
      false
    else
      loaded_on_type?(
        Map.get(record, resource_calculation.name),
        rest,
        resource_calculation.type,
        resource_calculation.constraints,
        opts
      )
    end
  end

  def loaded?(record, [%Ash.Resource.Attribute{} = attribute | rest], opts) do
    selected?(record, attribute.name) &&
      loaded_on_type?(
        Map.get(record, attribute.name),
        rest,
        attribute.type,
        attribute.constraints,
        opts
      )
  end

  def loaded?(%resource{} = record, [key | rest], opts) when is_atom(key) and not is_nil(key) do
    loaded?(record, [Ash.Resource.Info.field(resource, key) | rest], opts)
  end

  def loaded?(record, [%rel{name: name} | rest], opts)
      when rel in [
             Ash.Resource.Relationships.HasOne,
             Ash.Resource.Relationships.HasMany,
             Ash.Resource.Relationships.BelongsTo,
             Ash.Resource.Relationships.ManyToMany
           ] do
    record
    |> Map.get(name)
    |> loaded?(rest, opts)
  end

  def loaded?(_, _, opts), do: Keyword.get(opts, :unknown, false)

  defp loaded_on_type?(
         %Ash.NotLoaded{},
         _rest,
         _type,
         _constraints,
         _opts
       ) do
    false
  end

  defp loaded_on_type?(
         _,
         [],
         _type,
         _constraints,
         _opts
       ) do
    true
  end

  defp loaded_on_type?(empty, _, _, _, opts) when empty in [[], nil] do
    Keyword.get(opts, :unknown, false)
  end

  defp loaded_on_type?(value, path, type, constraints, opts) do
    Ash.Type.loaded?(type, value, path, constraints, opts)
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
      :__lateral_join_source__,
      :*,
      :calculations,
      :aggregates,
      :relationships,
      :as
    ]
  end

  @impl Spark.Dsl
  def explain(dsl_state, _) do
    Ash.Resource.Info.description(dsl_state)
  end
end
