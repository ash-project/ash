defmodule Ash.Resource do
  defmacro __using__(opts) do
    quote do
      @before_compile Ash.Resource

      Module.register_attribute(__MODULE__, :actions, accumulate: true)
      Module.register_attribute(__MODULE__, :attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :relationships, accumulate: true)

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

  defmacro __before_compile__(_env) do
    quote do
      if __MODULE__ not in Ash.resources() do
        raise "Your module (#{inspect(__MODULE__)}) must be in config, :ash, resources: [...]"
      end

      require Ash.Resource.Schema

      def type() do
        @resource_type
      end

      def relationships() do
        @relationships
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

      Ash.Resource.Schema.define_schema(@name)
    end
  end
end
