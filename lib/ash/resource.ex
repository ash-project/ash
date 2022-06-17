defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.Dsl`
  """

  @type t :: module
  @type record :: struct()

  use Ash.Dsl,
    single_extension_kinds: [:data_layer],
    many_extension_kinds: [
      :authorizers,
      :notifiers
    ],
    default_extensions: [
      data_layer: Ash.DataLayer.Simple,
      extensions: [Ash.Resource.Dsl]
    ]

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

  def handle_opts(opts) do
    quote bind_quoted: [embedded?: opts[:embedded?]] do
      if embedded? do
        @persist {:embedded?, true}

        require Ash.EmbeddableType

        Ash.EmbeddableType.define_embeddable_type()
      else
        use Ash.Type

        @impl true
        def storage_type, do: :map

        @impl Ash.Type
        def cast_input(nil, _), do: nil
        def cast_input(%struct{} = value, _) when struct == __MODULE__, do: {:ok, value}

        @impl Ash.Type
        def cast_stored(_, _),
          do:
            {:error,
             "Cannot cast a non embedded resource from storage. A non-embedded resource may only be used as an argument type."}

        @impl Ash.Type
        def dump_to_native(_, _),
          do:
            {:error,
             "Cannot dump a non embedded resource to native. A non-embedded resource may only be used as an argument type."}
      end
    end
  end

  def handle_before_compile(_opts) do
    quote do
      require Ash.Schema

      if !@moduledoc do
        @moduledoc Ash.Resource.Info.description(__MODULE__) || false
      end

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
                      |> Ash.Resource.Info.attributes()
                      |> Enum.map(& &1.name)
                      |> Enum.uniq()

      if AshPolicyAuthorizer.Authorizer in @extensions do
        raise """
        AshPolicyAuthorizer has been deprecated and is now built into Ash core.

        To use it, replace `authorizers: [AshPolicyAuthorizer.Authorizer]` with `authorizers: [Ash.Policy.Authorizer]`
        """
      end

      if api = Ash.Resource.Info.define_interface_for(__MODULE__) do
        require Ash.CodeInterface
        Ash.CodeInterface.define_interface(api, __MODULE__)
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
end
