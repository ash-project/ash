defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.DSL`

  For more information on the resource DSL, see `Ash.Resource.DSL`

  Note:
  *Do not* call the functions on a resource, as in `MyResource.type()` as this is a *private*
  API and can change at any time. Instead, use the `Ash` module, for example: `Ash.type(MyResource)`
  """

  @doc "A list of attribute names that make up the primary key, e.g [:class, :group]"
  @callback primary_key() :: [atom]
  @doc "A list of relationships to other resources"
  @callback relationships() :: [Ash.relationship()]
  @doc "A list of actions available for the resource"
  @callback actions() :: [Ash.action()]
  @doc "A list of attributes on the resource"
  @callback attributes() :: [Ash.attribute()]
  @doc "A list of extensions implemented by the resource"
  @callback extensions() :: [module]
  @doc "The data_layer in use by the resource, or nil if there is not one"
  @callback data_layer() :: module | nil
  @doc "A description of the resource, to be showed in generated documentation"
  @callback describe() :: String.t()
  @doc "A list of authorizers to be used when accessing the resource"
  @callback authorizers() :: [module]

  defmacro __using__(_opts) do
    quote do
      @before_compile Ash.Resource
      @behaviour Ash.Resource

      Ash.Resource.define_resource_module_attributes(__MODULE__)

      use Ash.Resource.DSL
    end
  end

  @doc false
  def define_resource_module_attributes(mod) do
    Module.register_attribute(mod, :before_compile_hooks, accumulate: true)
    Module.register_attribute(mod, :actions, accumulate: true)
    Module.register_attribute(mod, :attributes, accumulate: true)
    Module.register_attribute(mod, :relationships, accumulate: true)
    Module.register_attribute(mod, :extensions, accumulate: true)
    Module.register_attribute(mod, :authorizers, accumulate: true)

    Module.put_attribute(mod, :data_layer, nil)
    Module.put_attribute(mod, :description, nil)
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

      Ash.Schema.define_schema()

      def relationships do
        @relationships
      end

      def actions do
        @sanitized_actions
      end

      def attributes do
        @attributes
      end

      def primary_key do
        @ash_primary_key
      end

      def extensions do
        @extensions
      end

      def data_layer do
        @data_layer
      end

      def describe do
        @description
      end

      def authorizers do
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
