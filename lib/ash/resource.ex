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

      @attributes Ash.Resource.Attributes.Attribute.new(:id, :uuid)

      # Module.put_attribute(__MODULE__, :custom_threshold_for_lib, 10)
      import Ash.Resource
      import Ash.Resource.Actions, only: [actions: 1]
      import Ash.Resource.Attributes, only: [attributes: 1]
      import Ash.Resource.Relationships, only: [relationships: 1]

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

      def type() do
        @resource_type
      end

      def relationship(_name) do
        nil
      end

      def relationships() do
        @relationships
      end

      def action(_name) do
        nil
      end

      def actions() do
        @actions
      end

      def attributes() do
        @attributes
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
end
