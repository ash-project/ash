defmodule Ash.Resource.Relationships do
  @moduledoc """
  DSL components for declaring relationships.

  Relationships are a core component of resource oriented design. Many components of Ash
  will use these relationships. A simple use case is side_loading (done via the `side_load`
  option, given to an api action).
  """

  @doc false
  defmacro relationships(do: block) do
    quote do
      import Ash.Resource.Relationships

      unquote(block)

      import Ash.Resource.Relationships, only: [relationships: 1]
    end
  end

  defmodule HasOneDsl do
    require Ash.DslBuilder
    keys = Ash.Resource.Relationships.HasOne.opt_schema().opts -- [:name, :destination]

    Ash.DslBuilder.build_dsl(keys)
  end

  @doc """
  Declares a has_one relationship. In a relationsal database, the foreign key would be on the *other* table.

  Generally speaking, a `has_one` also implies that the destination table is unique on that foreign key.

  Practically speaking, a has_one and a belongs_to are interchangable in every way.

  #{Ashton.document(Ash.Resource.Relationships.HasOne.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  # In a resource called `Word`
  has_one :dictionary_entry, DictionaryEntry,
    source_field: :text,
    destination_field: :word_text
  ```

  """
  defmacro has_one(relationship_name, destination, opts \\ []) do
    quote do
      relationship_name = unquote(relationship_name)
      destination = unquote(destination)
      opts = unquote(Keyword.delete(opts, :do))

      unless is_atom(relationship_name) do
        raise Ash.Error.ResourceDslError,
          message: "relationship_name must be an atom",
          path: [:relationships, :has_one]
      end

      unless is_atom(destination) do
        raise Ash.Error.ResourceDslError,
          message: "related resource must be a module representing a resource",
          path: [:relationships, :has_one, relationship_name]
      end

      Module.register_attribute(__MODULE__, :dsl_opts, accumulate: true)
      import unquote(__MODULE__).HasOneDsl
      unquote(opts[:do])
      import unquote(__MODULE__).HasOneDsl, only: []

      opts = Keyword.merge(opts, @dsl_opts)

      Module.delete_attribute(__MODULE__, :dsl_opts)

      relationship =
        Ash.Resource.Relationships.HasOne.new(
          __MODULE__,
          @resource_type,
          relationship_name,
          destination,
          opts
        )

      case relationship do
        {:ok, relationship} ->
          @relationships relationship

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:relationships, :has_one, relationship_name]
      end
    end
  end

  defmodule BelongsToDsl do
    require Ash.DslBuilder
    keys = Ash.Resource.Relationships.BelongsTo.opt_schema().opts -- [:name, :destination]

    Ash.DslBuilder.build_dsl(keys)
  end

  @doc """
  Declares a belongs_to relationship. In a relational database, the foreign key would be on the *source* table.

  This creates a field on the resource with the corresponding name, unless `define_field?: false` is provided.

  Practically speaking, a belongs_to and a has_one are interchangable in every way.

  #{Ashton.document(Ash.Resource.Relationships.BelongsTo.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  # In a resource called `Word`
  belongs_to :dictionary_entry, DictionaryEntry,
    source_field: :text,
    destination_field: :word_text
  ```

  """
  defmacro belongs_to(relationship_name, destination, opts \\ []) do
    quote do
      relationship_name = unquote(relationship_name)
      destination = unquote(destination)
      opts = unquote(Keyword.delete(opts, :do))

      unless is_atom(relationship_name) do
        raise Ash.Error.ResourceDslError,
          message: "relationship_name must be an atom",
          path: [:relationships, :belongs_to]
      end

      unless is_atom(destination) do
        raise Ash.Error.ResourceDslError,
          message: "related resource must be a module representing a resource",
          path: [:relationships, :belongs_to, relationship_name]
      end

      Module.register_attribute(__MODULE__, :dsl_opts, accumulate: true)
      import unquote(__MODULE__).BelongsTo
      unquote(opts[:do])
      import unquote(__MODULE__).BelongsTo, only: []

      opts = Keyword.merge(opts, @dsl_opts)

      Module.delete_attribute(__MODULE__, :dsl_opts)

      relationship =
        Ash.Resource.Relationships.BelongsTo.new(
          __MODULE__,
          relationship_name,
          destination,
          opts
        )

      case relationship do
        {:ok, relationship} ->
          if relationship.define_field? do
            {:ok, attribute} =
              Ash.Resource.Attributes.Attribute.new(
                __MODULE__,
                relationship.source_field,
                relationship.field_type,
                primary_key?: relationship.primary_key?
              )

            @attributes attribute
          end

          @relationships relationship

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:relationships, :belongs_to, relationship_name]
      end
    end
  end

  defmodule HasManyDsl do
    require Ash.DslBuilder
    keys = Ash.Resource.Relationships.HasMany.opt_schema().opts -- [:name, :destination]

    Ash.DslBuilder.build_dsl(keys)
  end

  @doc """
  Declares a has_many relationship. There can be any number of related entities.

  #{Ashton.document(Ash.Resource.Relationships.HasMany.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  # In a resource called `Word`
  has_many :definitions, DictionaryDefinition,
    source_field: :text,
    destination_field: :word_text
  ```
  """
  defmacro has_many(relationship_name, destination, opts \\ []) do
    quote do
      relationship_name = unquote(relationship_name)
      destination = unquote(destination)
      opts = unquote(Keyword.delete(opts, :do))

      unless is_atom(relationship_name) do
        raise Ash.Error.ResourceDslError,
          message: "relationship_name must be an atom",
          path: [:relationships, :has_many]
      end

      unless is_atom(destination) do
        raise Ash.Error.ResourceDslError,
          message: "related resource must be a module representing a resource",
          path: [:relationships, :has_many, relationship_name]
      end

      Module.register_attribute(__MODULE__, :dsl_opts, accumulate: true)
      import unquote(__MODULE__).HasManyDsl
      unquote(opts[:do])
      import unquote(__MODULE__).HasManyDsl, only: []

      opts = Keyword.merge(opts, @dsl_opts)

      Module.delete_attribute(__MODULE__, :dsl_opts)

      relationship =
        Ash.Resource.Relationships.HasMany.new(
          __MODULE__,
          @resource_type,
          relationship_name,
          destination,
          opts
        )

      case relationship do
        {:ok, relationship} ->
          @relationships relationship

        {:error, [{key, message} | _]} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:relationships, :has_many, relationship_name]
      end
    end
  end

  defmodule ManyToManyDsl do
    require Ash.DslBuilder

    keys = Ash.Resource.Relationships.ManyToMany.opt_schema().opts -- [:name, :destination]

    Ash.DslBuilder.build_dsl(keys)
  end

  @doc """
  Declares a many_to_many relationship. Many to many relationships require a join table.

  A join table is typically a table who's primary key consists of one foreign key to each resource.

  You can specify a join table as a string or as another resource.

  #{Ashton.document(Ash.Resource.Relationships.ManyToMany.opt_schema(), header_depth: 2)}

  ## Examples
  ```elixir
  # In a resource called `Word`
  many_to_many :books, Book,
    through: BookWord,
    source_field: :text,
    source_field_on_join_table: :word_text,
    destination_field: :id,
    destination_field_on_join_table: :book_id
  ```
  """
  defmacro many_to_many(relationship_name, destination, opts \\ []) do
    quote do
      relationship_name = unquote(relationship_name)
      destination = unquote(destination)
      opts = unquote(Keyword.delete(opts, :do))

      Module.register_attribute(__MODULE__, :dsl_opts, accumulate: true)
      import unquote(__MODULE__).ManyToManyDsl
      unquote(opts[:do])
      import unquote(__MODULE__).ManyToManyDsl, only: []

      opts = Keyword.merge(opts, @dsl_opts)

      Module.delete_attribute(__MODULE__, :dsl_opts)

      many_to_many =
        Ash.Resource.Relationships.ManyToMany.new(
          __MODULE__,
          @name,
          relationship_name,
          destination,
          opts
        )

      # TODO: allow them to configure a join_relationship name
      # Update side loading logic to account for it.
      has_many_name = String.to_atom(to_string(relationship_name) <> "_join_assoc")

      has_many =
        Ash.Resource.Relationships.HasMany.new(
          __MODULE__,
          @name,
          has_many_name,
          opts[:through],
          destination_field: opts[:source_field_on_join_table],
          source_field: opts[:source_field] || :id
        )

      with {:many_to_many, {:ok, many_to_many}} <- {:many_to_many, many_to_many},
           {:has_many, {:ok, has_many}} <- {:has_many, has_many} do
        @relationships many_to_many
        @relationships has_many
      else
        {:many_to_many, {:error, [{key, message} | _]}} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:relationships, :many_to_many, relationship_name]

        {:has_many, {:error, [{key, message}]}} ->
          raise Ash.Error.ResourceDslError,
            message: message,
            option: key,
            path: [:relationships, :many_to_many, has_many_name]
      end
    end
  end
end
