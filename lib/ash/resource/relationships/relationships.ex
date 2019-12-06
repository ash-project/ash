defmodule Ash.Resource.Relationships do
  @moduledoc """
  DSL components for declaring relationships.

  Relationships are a core component of resource oriented design. Many components of Ash
  will use these relationships. A simple use case is side_loading (done via the `side_load`
  option, given to an api action). A more complex use case might be building authorization
  rules that grant access to a resource based on how the user is related to it.
  """

  @doc false
  defmacro relationships(do: block) do
    quote do
      import Ash.Resource.Relationships
      unquote(block)
      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end

  @doc """
  Declares a has_one relationship. In a relationsal database, the foreign key would be on the *other* table.

  Generally speaking, a `has_one` also implies that the destination table is unique on that foreign key.

  Practically speaking, a has_one and a belongs_to are interchangable in every way.

  Example:

  ```elixir
  # In a resource called `Word`
  has_one :dictionary_entry, DictionaryEntry,
    source_field: :text,
    destination_field: :word_text
  ```

  #{Ashton.document(Ash.Resource.Relationships.HasOne.opt_schema(), header_depth: 2)}
  """
  defmacro has_one(relationship_name, resource, opts \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.HasOne.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(opts)
        )

      case relationship do
        {:ok, relationship} ->
          @relationships relationship

        {:error, [{key, message}]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            resource: __MODULE__,
            path: [:relationships, :has_one, unquote(relationship_name)]
      end
    end
  end

  @doc """
  Declares a belongs_to relationship. In a relationsal database, the foreign key would be on the *source* table.

  Practically speaking, a belongs_to and a has_one are interchangable in every way.

  Example:

  ```elixir
  # In a resource called `Word`
  belongs_to :dictionary_entry, DictionaryEntry,
    source_field: :text,
    destination_field: :word_text
  ```

  #{Ashton.document(Ash.Resource.Relationships.BelongsTo.opt_schema(), header_depth: 2)}
  """
  defmacro belongs_to(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.BelongsTo.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      case relationship do
        {:ok, relationship} ->
          # TODO: This assumes binary_id
          @attributes Ash.Resource.Attributes.Attribute.new(
                        __MODULE__,
                        relationship.source_field,
                        :uuid,
                        primary_key?: relationship.primary_key?
                      )
          @relationships relationship

        {:error, [{key, message}]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            resource: __MODULE__,
            path: [:relationships, :belongs_to, unquote(relationship_name)]
      end
    end
  end

  @doc """
  Declares a has_many relationship. There can be any number of related entities.

  Example:

  ```elixir
  # In a resource called `Word`
  has_many :definitions, DictionaryDefinition,
    source_field: :text,
    destination_field: :word_text
  ```

  #{Ashton.document(Ash.Resource.Relationships.HasMany.opt_schema(), header_depth: 2)}
  """
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

      case relationship do
        {:ok, relationship} ->
          @relationships relationship

        {:error, [{key, message}]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            resource: __MODULE__,
            path: [:relationships, :has_many, unquote(relationship_name)]
      end
    end
  end

  @doc """
  Declares a many_to_many relationship. Many to many relationships require a join table.

  A join table is typically a table who's primary key consists of one foreign key to each resource.

  You can specify a join table as a string or as another resource.

  Example:

  ```elixir
  # In a resource called `Word`
  many_to_many :books, Book,
    through: BookWord,
    source_field: :text,
    source_field_on_join_table: :word_text,
    destination_field: :id,
    destination_field_on_join_table: :book_id
  ```

  #{Ashton.document(Ash.Resource.Relationships.ManyToMany.opt_schema(), header_depth: 2)}
  """
  defmacro many_to_many(relationship_name, resource, config \\ []) do
    quote do
      relationship =
        Ash.Resource.Relationships.ManyToMany.new(
          @name,
          unquote(relationship_name),
          unquote(resource),
          unquote(config)
        )

      case relationship do
        {:ok, relationship} ->
          @relationships relationship

        {:error, [{key, message}]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            resource: __MODULE__,
            path: [:relationships, :many_to_many, unquote(relationship_name)]
      end
    end
  end
end
