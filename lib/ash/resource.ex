defmodule Ash.Resource do
  defmacro __using__(opts) do
    quote do
      @before_compile Ash.Resource
      @skip_data_layer unquote(Keyword.get(opts, :no_data_layer, false))

      Module.register_attribute(__MODULE__, :before_compile_hooks, accumulate: true)

      Module.register_attribute(__MODULE__, :actions, accumulate: true)
      Module.register_attribute(__MODULE__, :attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :relationships, accumulate: true)
      Module.register_attribute(__MODULE__, :mix_ins, accumulate: true)

      if unquote(Keyword.get(opts, :primary_key?, true)) do
        @attributes Ash.Resource.Attributes.Attribute.new(:id, :uuid, primary_key?: true)
      end

      # Module.put_attribute(__MODULE__, :custom_threshold_for_lib, 10)
      import Ash.Resource
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Resource.Attributes, only: [attributes: 1]
      import Ash.Resource.Relationships, only: [relationships: 1]
      import Ash.Authorization.Rule

      name = unquote(opts[:name])
      resource_type = unquote(opts[:type])

      @name name
      @resource_type resource_type

      unless @name do
        raise "Must set name"
      end

      unless @resource_type do
        raise "Must set resource type"
      end
    end
  end

  defmacro __before_compile__(env) do
    quote do
      if __MODULE__ not in Ash.resources() do
        raise "Your module (#{inspect(__MODULE__)}) must be in config, :ash, resources: [...]"
      end

      @sanitized_actions Ash.Resource.mark_primaries(@actions)
      @ash_primary_key Ash.Resource.primary_key(@attributes)

      unless @ash_primary_key do
        raise "Must have a primary key for a resource: #{__MODULE__}"
      end

      def type() do
        @resource_type
      end

      def create(action, attributes, parameters) do
        data_layer().create(__MODULE__, action, attributes, parameters)
      end

      def relationship(name) do
        # TODO: Make this happen at compile time
        Enum.find(relationships(), &(&1.name == name))
      end

      def relationships() do
        @relationships
      end

      def action(name, type) do
        Enum.find(actions(), &(&1.name == name && &1.type == type))
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

      def mix_ins() do
        @mix_ins
      end

      unless @skip_data_layer || @data_layer do
        raise "Must `use` a data layer module or pass `no_data_layer: true`"
      end

      def data_layer() do
        if @skip_data_layer do
          false
        else
          @data_layer
        end
      end

      Enum.map(@mix_ins || [], fn hook_module ->
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
    |> case do
      [] ->
        nil

      [single] ->
        single

      other ->
        other
    end
  end

  @doc false
  def mark_primaries(all_actions) do
    all_actions
    |> Enum.group_by(& &1.type)
    |> Enum.flat_map(fn {type, actions} ->
      case actions do
        [action] ->
          [%{action | primary?: true}]

        actions ->
          case Enum.count(actions, & &1.primary?) do
            0 ->
              # TODO: Format these prettier
              raise "Must declare a primary action for #{type}, as there are more than one."

            1 ->
              actions

            _ ->
              raise "Duplicate primary actions declared for #{type}, but there can only be one primary action."
          end
      end
    end)
  end
end
