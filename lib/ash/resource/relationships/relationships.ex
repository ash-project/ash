defmodule Ash.Resource.Relationships do
  @moduledoc """
  DSL components for declaring relationships.

  Relationships are a core component of resource oriented design. Many components of Ash
  will use these relationships. A simple use case is side_loading (done via the `side_load`
  option, given to an api action). A more complex use case might be building authorization
  rules that grant access to a resource based on how the user is related to it.

  Available configurations:

  `has_one/3`
  `belongs_to/3`
  `has_many/3`
  `many_to_many/3`
  """
  defmacro relationships(do: block) do
    quote do
      import Ash.Resource.Relationships
      unquote(block)
      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end

  alias Ash.Resource.Relationships.HasOne

  @doc """
  Declares a has_one relationship. In a relationsal database, the foreign key would be on the *other* table.

  Generally speaking, a `has_one` also implies that the destination table is unique on that foreign key.

  Example:

  ```elixir
  # In a resource called `Word`
  has_one :dictionary_entry, DictionaryEntry,
    source_field: :text,
    destination_field: :word_text
  ```

  #{Ashton.document(HasOne.opt_schema())}
  """
  defmacro has_one(relationship_name, resource, opts \\ []) do
    quote do
      relationship =
        HasOne.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(opts)
        )

      @relationships relationship
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

      # TODO: This assumes binary_id
      @attributes Ash.Resource.Attributes.Attribute.new(
                    __MODULE__,
                    relationship.source_field,
                    :uuid,
                    primary_key?: relationship.primary_key?
                  )

      @relationships relationship
    end
  end

  defmacro has_many(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.HasMany.new(
          @name,
          @resource_type,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      @relationships relationship
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
    end
  end
end
