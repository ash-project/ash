defmodule Ash.EmbeddableType do
  @moduledoc false

  @embedded_resource_array_constraints [
    sort: [
      type: :any,
      doc: """
      A sort to be applied when casting the data.

      Only relevant for a type of {:array, `EmbeddedResource}`

      The sort is not applied when reading the data, so if the sort changes you will
      need to fix it in your database or wait for the data to be written again, at which
      point it will be sorted when casting.
      """
    ],
    load: [
      type: {:list, :atom},
      doc: """
      A list of calculations to load on the resource.

      Only relevant for a type of {:array, `EmbeddedResource}`

      Aggregates are not supported on embedded resources.
      """
    ],
    min_length: [
      type: :non_neg_integer,
      doc: "A minimum length for the items"
    ],
    max_length: [
      type: :non_neg_integer,
      doc: "A maximum length for the items"
    ],
    create_action: [
      type: :atom,
      doc:
        "The action to use on the resource when creating an embed. The primary is used by default."
    ],
    update_action: [
      type: :atom,
      doc:
        "The action to use on the resource when updating an embed. The primary is used by default."
    ],
    destroy_action: [
      type: :atom,
      doc:
        "The action to use on the resource when destroying an embed. The primary is used by default."
    ],
    read_action: [
      type: :atom,
      doc: "The read action to use when reading the embed. The primary is used by default."
    ],
    __source__: [
      type: :any,
      hide: true,
      doc:
        "This is hidden in the documentation, but this is used to add the `__source__` context to the changeset."
    ]
  ]

  defmodule ShadowApi do
    @moduledoc false
    use Ash.Api

    resources do
      allow_unregistered? true
    end

    execution do
      timeout :infinity
    end
  end

  @doc false
  def embedded_resource_array_constraints, do: @embedded_resource_array_constraints

  @doc false
  def handle_errors(errors) do
    errors
    |> do_handle_errors()
    |> List.wrap()
    |> Ash.Error.flatten_preserving_keywords()
  end

  defp do_handle_errors(errors) when is_list(errors) do
    if Keyword.keyword?(errors) do
      main_fields = Keyword.take(errors, [:message, :field, :fields, :path])
      vars = Keyword.merge(main_fields, Keyword.get(errors, :vars, []))

      main_fields
      |> Keyword.put(:vars, vars)
      |> Enum.into(%{})
      |> do_handle_errors()
    else
      Enum.map(errors, &do_handle_errors/1)
    end
  end

  defp do_handle_errors(%{errors: errors}) do
    errors
    |> List.wrap()
    |> do_handle_errors()
  end

  defp do_handle_errors(%Ash.Error.Changes.InvalidAttribute{
         message: message,
         field: field,
         vars: vars
       }) do
    vars
    |> Keyword.put(:field, field)
    |> Keyword.put(:message, message)
    |> add_index()
  end

  defp do_handle_errors(%{message: message, vars: vars, field: field}) do
    vars
    |> Keyword.put(:message, message)
    |> Keyword.put(:field, field)
    |> add_index()
  end

  defp do_handle_errors(%{message: message, vars: vars}) do
    vars
    |> Keyword.put(:message, message)
    |> add_index()
  end

  defp do_handle_errors(%{field: field} = exception) do
    [field: field, message: Exception.message(exception)]
  end

  defp do_handle_errors(error) when is_binary(error) do
    [message: error]
  end

  defp do_handle_errors(error) when is_exception(error) do
    [message: Exception.message(error)]
  end

  defp do_handle_errors(_error) do
    [message: "Something went wrong"]
  end

  defp add_index(opts) do
    opts
    # cond do
    #   opts[:index] && opts[:field] ->
    #     Keyword.put(opts, :field, "#{opts[:field]}[#{opts[:index]}]")

    #   opts[:index] ->
    #     Keyword.put(opts, :field, "[#{opts[:index]}]")

    #   true ->
    #     opts
    # end
  end

  defmacro single_embed_implementation do
    # credo:disable-for-next-line Credo.Check.Refactor.LongQuoteBlocks
    quote location: :keep do
      alias Ash.EmbeddableType.ShadowApi

      def storage_type, do: :map

      def cast_input(%{__struct__: __MODULE__} = input, _constraints), do: {:ok, input}

      def cast_input(value, constraints) when is_map(value) do
        action =
          constraints[:create_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :create).name

        __MODULE__
        |> Ash.Changeset.new()
        |> Ash.Changeset.set_context(%{__source__: constraints[:__source__]})
        |> Ash.Changeset.for_create(action, value)
        |> ShadowApi.create()
        |> case do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.EmbeddableType.handle_errors(error)}
        end
      end

      def cast_input(_, _), do: :error

      def cast_stored(value, constraints) when is_map(value) do
        __MODULE__
        |> Ash.Resource.Info.attributes()
        |> Enum.reduce_while({:ok, struct(__MODULE__)}, fn attr, {:ok, struct} ->
          with {:fetch, {:ok, value}} <- {:fetch, fetch_key(value, attr.name)},
               {:ok, casted} <-
                 Ash.Type.cast_stored(attr.type, value, attr.constraints) do
            {:cont, {:ok, Map.put(struct, attr.name, casted)}}
          else
            {:fetch, :error} ->
              {:cont, {:ok, struct}}

            other ->
              {:halt, other}
          end
        end)
        |> case do
          {:ok, casted} ->
            case constraints[:load] do
              empty when empty in [nil, []] ->
                {:ok, casted}

              load ->
                action =
                  constraints[:read_action] ||
                    Ash.Resource.Info.primary_action!(__MODULE__, :read).name

                __MODULE__
                |> Ash.DataLayer.Simple.set_data([casted])
                |> Ash.Query.load(load)
                |> Ash.Query.for_read(action)
                |> ShadowApi.read()
                |> case do
                  {:ok, [casted]} ->
                    {:ok, casted}

                  {:error, errors} ->
                    {:error, Ash.EmbeddableType.handle_errors(errors)}
                end
            end

          other ->
            other
        end
      end

      def cast_stored(nil, _), do: {:ok, nil}

      def cast_stored(_other, _) do
        :error
      end

      def fetch_key(map, atom) do
        case Map.fetch(map, atom) do
          {:ok, value} ->
            {:ok, value}

          :error ->
            Map.fetch(map, to_string(atom))
        end
      end

      def dump_to_native(value, _) when is_map(value) do
        attributes = Ash.Resource.Info.attributes(__MODULE__)

        Enum.reduce_while(attributes, {:ok, %{}}, fn attribute, {:ok, acc} ->
          case Map.fetch(value, attribute.name) do
            :error ->
              {:cont, {:ok, acc}}

            {:ok, value} ->
              case Ash.Type.dump_to_embedded(
                     attribute.type,
                     value,
                     Map.get(attribute, :constraints) || []
                   ) do
                :error ->
                  {:halt, :error}

                {:ok, dumped} ->
                  {:cont, {:ok, Map.put(acc, attribute.name, dumped)}}
              end
          end
        end)
      end

      def dump_to_native(nil, _), do: {:ok, nil}
      def dump_to_native(_, _), do: :error

      def constraints,
        do:
          Keyword.take(array_constraints(), [
            :load,
            :create_action,
            :destroy_action,
            :update_action,
            :__source__
          ])

      def apply_constraints(nil, _), do: {:ok, nil}

      def apply_constraints(term, constraints) do
        ShadowApi.load(term, constraints[:load] || [])
      end

      def handle_change(nil, new_value, _constraints) do
        {:ok, new_value}
      end

      def handle_change(old_value, nil, constraints) do
        action =
          constraints[:destroy_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :destroy).name

        case ShadowApi.destroy(old_value, action: action) do
          :ok -> {:ok, nil}
          {:error, error} -> {:error, Ash.EmbeddableType.handle_errors(error)}
        end
      end

      def handle_change(old_value, new_value, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.Info.attribute(__MODULE__, pkey_field).private?
           end) do
          {:ok, new_value}
        else
          pkey = Map.take(old_value, pkey_fields)

          if Map.take(new_value, pkey_fields) == pkey do
            {:ok, new_value}
          else
            action =
              constraints[:destroy_action] ||
                Ash.Resource.Info.primary_action!(__MODULE__, :destroy).name

            case ShadowApi.destroy(old_value, action: action) do
              :ok ->
                {:ok, new_value}

              {:error, error} ->
                {:error, Ash.EmbeddableType.handle_errors(error)}
            end
          end
        end
      end

      def prepare_change(old_value, "", constraints) do
        prepare_change(old_value, nil, constraints)
      end

      def prepare_change(_old_value, nil, _constraints) do
        {:ok, nil}
      end

      def prepare_change(_old_value, %{__struct__: __MODULE__} = new_value, _constraints) do
        {:ok, new_value}
      end

      def prepare_change(nil, new_value, _constraints) do
        {:ok, new_value}
      end

      def prepare_change(old_value, new_uncasted_value, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.Info.attribute(__MODULE__, pkey_field).private?
           end) do
          action =
            constraints[:update_action] ||
              Ash.Resource.Info.primary_action!(__MODULE__, :update).name

          old_value
          |> Ash.Changeset.new()
          |> Ash.Changeset.set_context(%{__source__: constraints[:__source__]})
          |> Ash.Changeset.for_update(action, new_uncasted_value)
          |> ShadowApi.update()
          |> case do
            {:ok, value} -> {:ok, value}
            {:error, error} -> {:error, Ash.EmbeddableType.handle_errors(error)}
          end
        else
          pkey =
            Enum.into(pkey_fields, %{}, fn pkey_field ->
              case fetch_key(new_uncasted_value, pkey_field) do
                :error ->
                  {pkey_field, :error}

                {:ok, value} ->
                  attribute = Ash.Resource.Info.attribute(__MODULE__, pkey_field)

                  case Ash.Type.cast_input(attribute.type, value, attribute.constraints) do
                    {:ok, casted} ->
                      {pkey_field, casted}

                    _ ->
                      {pkey_field, :error}
                  end
              end
            end)

          if Enum.any?(Map.values(pkey), &(&1 == :error)) do
            {:ok, new_uncasted_value}
          else
            old_pkey = Map.take(old_value, pkey_fields)

            if old_pkey == pkey do
              action =
                constraints[:update_action] ||
                  Ash.Resource.Info.primary_action!(__MODULE__, :update).name

              old_value
              |> Ash.Changeset.new()
              |> Ash.Changeset.set_context(%{__source__: constraints[:__source__]})
              |> Ash.Changeset.for_update(action, new_uncasted_value)
              |> ShadowApi.update()
              |> case do
                {:ok, value} -> {:ok, value}
                {:error, error} -> {:error, Ash.EmbeddableType.handle_errors(error)}
              end
            else
              {:ok, new_uncasted_value}
            end
          end
        end
      end
    end
  end

  defmacro array_embed_implementation do
    # credo:disable-for-next-line Credo.Check.Refactor.LongQuoteBlocks
    quote location: :keep do
      alias Ash.EmbeddableType.ShadowApi
      def array_constraints, do: Ash.EmbeddableType.embedded_resource_array_constraints()

      def apply_constraints_array(term, constraints) do
        pkey = Ash.Resource.Info.primary_key(__MODULE__)
        unique_keys = Enum.map(Ash.Resource.Info.identities(__MODULE__), & &1.keys) ++ [pkey]

        case find_duplicates(term, unique_keys) do
          nil ->
            query =
              __MODULE__
              |> Ash.DataLayer.Simple.set_data(term)
              |> Ash.Query.load(constraints[:load] || [])

            query =
              if constraints[:sort] do
                Ash.Query.sort(query, constraints[:sort])
              else
                query
              end

            case ShadowApi.read(query) do
              {:ok, result} ->
                case Ash.Type.list_constraint_errors(result, constraints) do
                  [] ->
                    {:ok, result}

                  errors ->
                    {:error, errors}
                end
            end

          keys ->
            {:error, message: "items must be unique on keys %{keys}", keys: Enum.join(keys, ",")}
        end
      end

      defp find_duplicates(term, unique_keys) do
        Enum.find(unique_keys, fn unique_key ->
          has_duplicates?(term, __MODULE__, fn item ->
            Enum.reduce(unique_key, %{}, fn key, acc ->
              if Map.has_key?(item, key) || Map.has_key?(item, to_string(key)) do
                attribute = Ash.Resource.Info.attribute(__MODULE__, key)

                case Ash.Type.cast_input(
                       attribute.type,
                       Map.get(item, key) || Map.get(item, to_string(key)),
                       attribute.constraints
                     ) do
                  {:ok, value} ->
                    Map.put(acc, key, value)

                  _ ->
                    acc
                end
              end
            end)
          end)
        end)
      end

      defp has_duplicates?(list, resource, func) do
        list
        |> Enum.reduce_while(MapSet.new(), fn x, acc ->
          x = func.(x)

          acc
          |> Enum.any?(fn item ->
            Enum.all?(x, fn {key, value} ->
              attr = Ash.Resource.Info.attribute(resource, key)

              Enum.any?(acc, fn other ->
                Ash.Type.equal?(attr.type, Map.get(other, key), value)
              end)
            end)
          end)
          |> case do
            true ->
              {:halt, 0}

            false ->
              {:cont, MapSet.put(acc, x)}
          end
        end)
        |> is_integer()
      end

      def handle_change_array(nil, new_values, constraints) do
        handle_change_array([], new_values, constraints)
      end

      def handle_change_array(old_values, nil, constraints) do
        handle_change_array(old_values, [], constraints)
      end

      def handle_change_array(old_values, new_values, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        destroy_action =
          constraints[:destroy_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :destroy).name

        old_values
        |> Enum.with_index()
        |> Enum.reject(fn {old_value, _} ->
          pkey = Map.take(old_value, pkey_fields)

          Enum.any?(new_values, fn new_value ->
            Map.take(new_value, pkey_fields) == pkey
          end)
        end)
        |> Enum.reduce_while(:ok, fn {record, index}, :ok ->
          case ShadowApi.destroy(record, action: destroy_action) do
            :ok ->
              {:cont, :ok}

            {:error, error} ->
              errors =
                error
                |> Ash.EmbeddableType.handle_errors()
                |> Enum.map(fn keyword ->
                  Keyword.put(keyword, :index, index)
                end)

              {:halt, {:error, errors}}
          end
        end)
        |> case do
          :ok ->
            {:ok, new_values}

          {:error, error} ->
            {:error, error}
        end
      end

      def prepare_change_array(old_values, new_uncasted_values, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        if Enum.all?(pkey_fields, fn pkey_field ->
             Ash.Resource.Info.attribute(__MODULE__, pkey_field).private?
           end) do
          {:ok, new_uncasted_values}
        else
          pkey_attributes =
            Enum.into(pkey_fields, %{}, fn field ->
              {field, Ash.Resource.Info.attribute(__MODULE__, field)}
            end)

          action =
            constraints[:update_action] ||
              Ash.Resource.Info.primary_action!(__MODULE__, :update).name

          new_uncasted_values
          |> Enum.with_index()
          |> Enum.reduce_while(
            {:ok, []},
            fn
              {new, _index}, {:ok, new_uncasted_values} when is_struct(new, __MODULE__) ->
                {:cont, {:ok, [new | new_uncasted_values]}}

              {new, index}, {:ok, new_uncasted_values} ->
                pkey =
                  Enum.into(pkey_fields, %{}, fn pkey_field ->
                    case fetch_key(new, pkey_field) do
                      :error ->
                        {pkey_field, :error}

                      {:ok, value} ->
                        attr = Map.get(pkey_attributes, pkey_field)

                        case Ash.Type.cast_input(attr.type, value, attr.constraints) do
                          {:ok, casted} ->
                            {pkey_field, casted}

                          _ ->
                            {pkey_field, :error}
                        end
                    end
                  end)

                if Enum.any?(Map.values(pkey), &(&1 == :error)) do
                  {:cont, {:ok, [new | new_uncasted_values]}}
                else
                  value_updating_from =
                    Enum.find(old_values, fn old_value ->
                      Map.take(old_value, pkey_fields) == pkey
                    end)

                  if value_updating_from do
                    value_updating_from
                    |> Ash.Changeset.new()
                    |> Ash.Changeset.set_context(%{__source__: constraints[:__source__]})
                    |> Ash.Changeset.for_update(action, new)
                    |> ShadowApi.update()
                    |> case do
                      {:ok, value} ->
                        {:cont, {:ok, [value | new_uncasted_values]}}

                      {:error, error} ->
                        errors =
                          error
                          |> Ash.EmbeddableType.handle_errors()
                          |> Enum.map(fn keyword ->
                            Keyword.put(keyword, :index, index)
                          end)

                        {:halt, {:error, errors}}
                    end
                  else
                    {:cont, {:ok, [new | new_uncasted_values]}}
                  end
                end
            end
          )
          |> case do
            {:ok, values} -> {:ok, Enum.reverse(values)}
            {:error, error} -> {:error, error}
          end
        end
      end
    end
  end

  defmacro define_embeddable_type do
    quote location: :keep do
      use Ash.Type, embedded?: true

      Ash.EmbeddableType.single_embed_implementation()
      Ash.EmbeddableType.array_embed_implementation()
    end
  end
end
