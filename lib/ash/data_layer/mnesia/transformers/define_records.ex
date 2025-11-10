defmodule Ash.DataLayer.Mnesia.Transformers.DefineRecords do
  @moduledoc """
  Generates Erlang record definitions for Mnesia resources at compile time.

  This transformer reads the resource's attributes and creates an Erlang record
  definition using Record.defrecord, making Mnesia operations more efficient and
  enabling pattern matching on records.

  For compound primary keys, the first field in the record will be a tuple
  containing all primary key values. It is recommended to specify all of the
  primary keys first and then normal attributes. It is also recommended to leave
  the ordering as static as possible. If you are rebuilding the Mnesia tables
  every time, it doesn't matter.
  """

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  @doc false
  def transform(dsl_state) do
    resource = Transformer.get_persisted(dsl_state, :module)
    attributes = Ash.Resource.Info.attributes(dsl_state)
    primary_key = Ash.Resource.Info.primary_key(dsl_state)

    # Get table name from mnesia configuration or default to resource name
    table = get_table_name(dsl_state, resource)

    # Build field list with defaults for the record
    fields =
      build_field_list(attributes, primary_key)

    fields =
      if length(fields) == 1 do
        fields ++ [{:__meta__, nil}]
      else
        fields
      end

    # Inject the record definition and helper functions
    dsl_state = inject_record_code(dsl_state, table, fields, attributes, primary_key)

    {:ok, dsl_state}
  end

  defp get_table_name(dsl_state, resource) do
    case Ash.DataLayer.Mnesia.Info.table(dsl_state) do
      table when is_atom(table) ->
        table

      _ ->
        resource
    end
  end

  defp build_field_list(attributes, primary_key) do
    # For compound primary keys (length > 1), we need to structure the record differently
    # The first field will be a tuple of the PK values
    if length(primary_key) > 1 do
      pk_tuple = primary_key |> Enum.map(fn _ -> nil end) |> List.to_tuple()

      non_pk_attrs =
        Enum.reject(attributes, & &1.primary_key?)
        |> Enum.map(fn attr -> {attr.name, nil} end)

      [{:_pkey, pk_tuple} | non_pk_attrs]
    else
      Enum.map(attributes, &{&1.name, nil})
    end
  end

  # This is a bit tricky of a function so let me explain...
  # Since Elixir/Erlang Records are compile time contructs, it is not easy to
  # dynamically define a subset of them at runtime. For instance, if you sepcify
  # a `Record.defrecord(:mytable, [a: nil, b: nil])`, you can explicitly create
  # the record with a subset of the attribs like `mytable(b: "foo")`. This
  # doesn't work in macros very well though. This function is creating the AST
  # for assigning records at runtime. So if you do a `resource.to_ex_record(%{b:
  # "foo"})` it will fill in the missing fields correctly. This just adds
  # defaults of `nil` because `Ash.Resource` is handling defaults, but we could
  # extend this to handle other types of defaults as well.
  defp build_field_assignments(field_names, is_compound_key, attributes, primary_key) do
    if is_compound_key do
      # First field is the compound key tuple
      pk_tuple_assignment =
        quote do
          {
            unquote_splicing(
              Enum.with_index(primary_key, 1)
              |> Enum.map(fn {pk, idx} ->
                expand_value(pk, idx)
              end)
            )
          }
        end

      # Non-PK field assignments
      non_pk_fields = Enum.reject(attributes, &(&1.name in primary_key))

      non_pk_assignments =
        Enum.with_index(non_pk_fields, length(primary_key) + 1)
        |> Enum.map(fn {attr, idx} ->
          {attr.name, expand_value(attr.name, idx)}
        end)

      [{:_pkey, pk_tuple_assignment} | non_pk_assignments]
    else
      Enum.with_index(field_names, 1)
      |> Enum.map(fn {field, idx} ->
        # {field, quote(do: Map.get(attrs, unquote(field), default))}
        {field, expand_value(field, idx)}
      end)
    end
  end

  defp expand_value(field, idx) do
    quote do
      if default == :position do
        Map.get(attrs, unquote(field), :"$#{unquote(idx)}")
      else
        Map.get(attrs, unquote(field), default)
      end
    end
  end

  defp inject_record_code(dsl_state, table, fields, attributes, primary_key) do
    is_compound_key = length(primary_key) > 1
    field_names = Enum.map(attributes, & &1.name)

    field_assignments =
      build_field_assignments(field_names, is_compound_key, attributes, primary_key)

    Transformer.eval(
      dsl_state,
      [
        table: table,
        fields: fields,
        field_names: field_names,
        primary_key: primary_key,
        is_compound_key: is_compound_key,
        field_assignments: field_assignments
      ],
      quote do
        require Record

        Record.defrecordp(unquote(table), unquote(fields))

        # Store field defaults as module attribute
        @is_compound_key unquote(is_compound_key)
        @primary_key_fields unquote(primary_key)

        @doc """
        Returns the field names for the Mnesia record, suitable for use with
        `Mnesia.create_table/2` as the `:attributes` option.

        For compound primary keys, returns `[:_pkey | non_pk_fields]`.
        For simple primary keys, returns all attribute names.

        ## Examples

            iex> #{inspect(__MODULE__)}.mnesia_record_info()
            [:id, :name, :age, :email]
        """
        def mnesia_record_info do
          if @is_compound_key do
            non_pk_fields = Enum.reject(unquote(field_names), &(&1 in @primary_key_fields))

            case [:_pkey | non_pk_fields] do
              [:_pkey] -> [:_pkey, :_meta]
              fields -> fields
            end
          else
            unquote(field_names)
          end
        end

        @doc """
        Creates a new #{unquote(table)} record from a map or keyword list.

        ## Examples

            iex> #{inspect(__MODULE__)}.to_ex_record(id: 1, name: "Alice")
            {#{inspect(unquote(table))}, 1, "Alice", ...}

            iex> #{inspect(__MODULE__)}.to_ex_record(%{id: 1, name: "Alice"})
            {#{inspect(unquote(table))}, 1, "Alice", ...}
        """

        def to_ex_record(attrs, default \\ nil)

        def to_ex_record(attrs, default) when is_list(attrs) do
          to_ex_record(Map.new(attrs), default)
        end

        def to_ex_record(attrs, default) when is_map(attrs) do
          unquote(table)(unquote(field_assignments))
        end

        @doc """
        Converts a #{unquote(table)} record to a map.

        ## Examples

            iex> record = #{inspect(__MODULE__)}.to_ex_record(id: 1, name: "Alice")
            iex> #{inspect(__MODULE__)}.record_to_map(record)
            %{id: 1, name: "Alice", ...}
        """
        def record_to_map(record) when is_tuple(record) do
          # Skip the first element (record tag, i.e. table name)
          [_tag | values] = Tuple.to_list(record)

          if @is_compound_key do
            # First value is the compound key tuple
            [pk_tuple | rest_values] = values

            pk_values = Tuple.to_list(pk_tuple)

            pk_map =
              @primary_key_fields
              |> Enum.zip(pk_values)
              |> Enum.into(%{})

            non_pk_fields = Enum.reject(unquote(field_names), &(&1 in @primary_key_fields))

            non_pk_map =
              non_pk_fields
              |> Enum.zip(rest_values)
              |> Enum.into(%{})

            Map.merge(pk_map, non_pk_map)
          else
            # Simple key - same as before
            field_names = unquote(field_names)

            field_names
            |> Enum.zip(values)
            |> Enum.into(%{})
          end
        end

        @doc """
        Converts a #{unquote(table)} record to an Ash Resource struct.

        ## Examples

            iex> record = #{inspect(__MODULE__)}.to_ex_record(id: 1, name: "Alice")
            iex> #{inspect(__MODULE__)}.from_ex_record(record)
            %#{inspect(__MODULE__)}{id: 1, name: "Alice", ...}
        """
        def from_ex_record(record) when is_tuple(record) do
          record
          |> record_to_map()
          |> then(&struct(__MODULE__, &1))
        end
      end
    )
  end
end
