defmodule Ash.Resource.Relationships do
  defmacro relationships(do: block) do
    quote do
      import Ash.Resource.Relationships
      unquote(block)
      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end

  defmacro has_one(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.HasOne.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      @relationships relationship
      @current_relationship relationship

      def relationship(unquote(relationship_name)) do
        @current_relationship
      end
    end
  end

  defmacro belongs_to(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.BelongsTo.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      @relationships relationship
      @current_relationship relationship

      def relationship(unquote(relationship_name)) do
        @current_relationship
      end
    end
  end

  defmacro has_many(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.HasMany.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      @relationships relationship
      @current_relationship relationship

      def relationship(unquote(relationship_name)) do
        @current_relationship
      end
    end
  end

  defmacro many_to_many(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.ManyToMany.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      @relationships relationship
      @current_relationship relationship

      def relationship(unquote(relationship_name)) do
        @current_relationship
      end
    end
  end
end
