defmodule Ash.Resource do
  @resource_opts_schema [
    name: [
      type: :string,
      required: true,
      doc:
        "The name of the resource, e.g `posts` or `authors`. This will typically be the pluralized form of the type"
    ],
    type: [
      type: :string,
      required: true,
      doc: "The type of the resource, e.g `post` or `author`. This is used throughout the system."
    ]
  ]

  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.DSL`

  #{NimbleOptions.docs(@resource_opts_schema)}

  Note:
  *Do not* call the functions on a resource, as in `MyResource.type()` as this is a *private*
  API and can change at any time. Instead, use the `Ash` module, for example: `Ash.type(MyResource)`
  """

  @callback primary_key() :: [atom]

  defmacro __using__(opts) do
    quote do
      @before_compile Ash.Resource
      @behaviour Ash.Resource

      opts =
        case NimbleOptions.validate(unquote(opts), Ash.Resource.resource_opts_schema()) do
          {:error, message} ->
            raise Ash.Error.ResourceDslError,
              using: __MODULE__,
              message: message

          {:ok, opts} ->
            opts
        end

      Ash.Resource.define_resource_module_attributes(__MODULE__, opts)

      use Ash.Resource.DSL
    end
  end

  @doc false
  def define_resource_module_attributes(mod, opts) do
    Module.register_attribute(mod, :before_compile_hooks, accumulate: true)
    Module.register_attribute(mod, :actions, accumulate: true)
    Module.register_attribute(mod, :attributes, accumulate: true)
    Module.register_attribute(mod, :relationships, accumulate: true)
    Module.register_attribute(mod, :extensions, accumulate: true)
    Module.register_attribute(mod, :authorizers, accumulate: true)

    Module.put_attribute(mod, :name, opts[:name])
    Module.put_attribute(mod, :resource_type, opts[:type])
    Module.put_attribute(mod, :data_layer, nil)
    Module.put_attribute(mod, :description, nil)
  end

  @doc false
  def resource_opts_schema() do
    @resource_opts_schema
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro __before_compile__(env) do
    quote do
      case Ash.Resource.mark_primaries(@actions) do
        {:ok, actions} ->
          @sanitized_actions actions

        {:error, {:no_primary, type}} ->
          raise Ash.Error.ResourceDslError,
            message:
              "Multiple actions of type #{type} defined, one must be designated as `primary?: true`",
            path: [:actions, type]

        {:error, {:duplicate_primaries, type}} ->
          raise Ash.Error.ResourceDslError,
            message:
              "Multiple actions of type #{type} configured as `primary?: true`, but only one action per type can be the primary",
            path: [:actions, type]
      end

      @ash_primary_key Ash.Resource.primary_key(@attributes)

      require Ash.Schema

      Ash.Schema.define_schema(@name)

      def type() do
        @resource_type
      end

      def relationships() do
        @relationships
      end

      def actions() do
        @sanitized_actions
      end

      def attributes() do
        @attributes
      end

      def primary_key() do
        @ash_primary_key
      end

      def name() do
        @name
      end

      def extensions() do
        @extensions
      end

      def data_layer() do
        @data_layer
      end

      def describe() do
        @description
      end

      def authorizers() do
        @authorizers
      end

      Enum.map(@extensions || [], fn hook_module ->
        code = hook_module.before_compile_hook(unquote(Macro.escape(env)))
        Module.eval_quoted(__MODULE__, code)
      end)
    end
  end

  @doc false
  def primary_key(attributes) do
    attributes
    |> Enum.filter(& &1.primary_key?)
    |> Enum.map(& &1.name)
  end

  @doc false
  def mark_primaries(all_actions) do
    actions =
      all_actions
      |> Enum.group_by(& &1.type)
      |> Enum.flat_map(fn {type, actions} ->
        case actions do
          [action] ->
            [%{action | primary?: true}]

          actions ->
            check_primaries(actions, type)
        end
      end)

    Enum.find(actions, fn action -> match?({:error, _}, action) end) || {:ok, actions}
  end

  defp check_primaries(actions, type) do
    case Enum.count(actions, & &1.primary?) do
      0 ->
        [{:error, {:no_primary, type}}]

      1 ->
        actions

      _ ->
        [{:error, {:duplicate_primaries, type}}]
    end
  end
end
