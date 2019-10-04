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
      @relationships Ash.Resource.Relationships.HasOne.new(
                       @name,
                       unquote(relationship_name),
                       unquote(resource),
                       unquote(config)
                     )
    end
  end

  defmacro belongs_to(relationship_name, resource, config \\ []) do
    quote do
      @relationships Ash.Resource.Relationships.BelongsTo.new(
                       @name,
                       unquote(relationship_name),
                       unquote(resource),
                       unquote(config)
                     )
    end
  end

  # defmacro has_many(name, resource, config \\ []) do
  #   quote do
  #     @relationships Keyword.put(@relationships, unquote(name),
  #                      type: :has_many,
  #                      resource: unquote(resource),
  #                      config: unquote(config)
  #                    )
  #   end
  # end

  # defmacro belongs_to(name, resource, config \\ []) do
  #   quote do
  #     @relationships Keyword.put(@relationships, unquote(name),
  #                      type: :belongs_to,
  #                      resource: unquote(resource),
  #                      config: unquote(config)
  #                    )
  #   end
  # end

  # defmacro many_to_many(name, resource, config \\ []) do
  #   quote do
  #     @relationships Keyword.put(@relationships, unquote(name),
  #                      type: :many_to_many,
  #                      resource: unquote(resource),
  #                      config: unquote(config)
  #                    )
  #   end
  # end
end
