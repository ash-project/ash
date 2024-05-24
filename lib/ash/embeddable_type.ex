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
    domain: [
      type: {:spark, Ash.Domain},
      doc:
        "The domain to use when interacting with the resource. Defaults to the domain of the source changeset."
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

  defmodule ShadowDomain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered?(true)
    end

    execution do
      timeout(:infinity)
    end
  end

  @doc false
  def embedded_resource_array_constraints, do: @embedded_resource_array_constraints

  @doc false
  def handle_errors(errors) do
    errors
    |> do_handle_errors()
    |> List.wrap()
    |> Ash.Helpers.flatten_preserving_keywords()
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
  end

  defp do_handle_errors(%{message: message, vars: vars, field: field}) do
    vars
    |> Keyword.put(:message, message)
    |> Keyword.put(:field, field)
  end

  defp do_handle_errors(%{message: message, vars: vars}) do
    vars
    |> Keyword.put(:message, message)
  end

  defp do_handle_errors(error) when is_binary(error) do
    [message: error]
  end

  defp do_handle_errors(error) when is_exception(error) do
    [error]
  end

  defp do_handle_errors(_error) do
    [message: "Something went wrong"]
  end

  defmacro single_embed_implementation(opts) do
    # credo:disable-for-next-line Credo.Check.Refactor.LongQuoteBlocks
    quote generated: true, location: :keep, bind_quoted: [opts: opts] do
      alias Ash.EmbeddableType.ShadowDomain

      def storage_type(_), do: :map

      def cast_atomic(value, constraints) do
        if Ash.Expr.expr?(value) do
          if Enum.empty?(Ash.Resource.Info.primary_key(__MODULE__)) ||
               constraints[:on_update] == :replace do
            case cast_input(value, constraints) do
              {:ok, value} ->
                {:atomic, value}

              {:error, error} ->
                {:error, error}
            end
          else
            {:not_atomic,
             "Embedded attributes do not support atomic updates unless they have no primary key, or `constraints[:on_update]` is set to `:replace`."}
          end
        else
          {:not_atomic,
           "Embedded attributes do not support atomic updates with expressions, only literal values."}
        end
      end

      def cast_input(%{__struct__: __MODULE__} = input, _constraints), do: {:ok, input}

      def cast_input(value, constraints) when is_map(value) do
        action =
          constraints[:create_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :create).name

        __MODULE__
        |> Ash.Changeset.new()
        |> Ash.EmbeddableType.copy_source(constraints)
        |> Ash.Changeset.for_create(action, value,
          domain: ShadowDomain,
          skip_unknown_inputs: skip_unknown_inputs(constraints)
        )
        |> Ash.create()
        |> case do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.EmbeddableType.handle_errors(error)}
        end
      end

      def cast_input(nil, _), do: {:ok, nil}

      def cast_input(_, _), do: :error

      def cast_input_array(value, constraints) when is_list(value) do
        action =
          constraints[:create_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :create).name

        {structs, values} =
          value
          |> Stream.with_index()
          |> Enum.split_with(&is_struct(elem(&1, 0), __MODULE__))

        {context, opts} =
          case constraints[:__source__] do
            %Ash.Changeset{context: context} = source ->
              {Map.put(context, :__source__, source),
               Ash.Context.to_opts(context[:private] || %{})}

            _ ->
              {%{}, []}
          end

        values
        |> Stream.map(&elem(&1, 0))
        |> Ash.bulk_create(
          __MODULE__,
          action,
          Keyword.merge(opts,
            domain: ShadowDomain,
            context: context,
            sorted?: true,
            skip_unknown_inputs: skip_unknown_inputs(constraints),
            return_records?: true,
            return_errors?: true,
            batch_size: 1_000_000_000
          )
        )
        |> case do
          %{status: :success, records: records} ->
            Enum.reduce(structs, {:ok, records}, fn {struct, index}, {:ok, records} ->
              {:ok, List.insert_at(records, index, struct)}
            end)

          %{errors: errors} ->
            errors =
              Enum.map(errors, fn
                %Ash.Changeset{context: %{bulk_create: %{index: index}}, errors: errors} ->
                  Ash.Error.set_path(Ash.Error.to_ash_error(errors), index)

                other ->
                  Ash.Error.to_ash_error(other)
              end)

            {:error, Ash.Error.to_ash_error(errors)}
        end
      end

      def cast_input_array(nil, _), do: {:ok, nil}

      def cast_input_array(_, _), do: :error

      def cast_stored(value, constraints) when is_map(value) do
        __MODULE__
        |> Ash.Resource.Info.attributes()
        |> Enum.reduce_while({:ok, struct(__MODULE__)}, fn attr, {:ok, struct} ->
          with {:fetch, {:ok, value}} <- {:fetch, fetch_key(value, attr.source)},
               {:ok, casted} <- Ash.Type.cast_stored(attr.type, value, attr.constraints) do
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
                |> Ash.Query.for_read(action, %{}, domain: ShadowDomain)
                |> Ash.read()
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
              value =
                case value do
                  %Ash.ForbiddenField{original_value: original_value} ->
                    original_value

                  other ->
                    other
                end

              case Ash.Type.dump_to_embedded(
                     attribute.type,
                     value,
                     Map.get(attribute, :constraints) || []
                   ) do
                :error ->
                  {:halt, :error}

                {:ok, nil} when unquote(!opts[:embed_nil_values?]) ->
                  {:cont, {:ok, acc}}

                {:ok, dumped} ->
                  {:cont, {:ok, Map.put(acc, attribute.source, dumped)}}
              end
          end
        end)
      end

      def dump_to_native(nil, _), do: {:ok, nil}
      def dump_to_native(_, _), do: :error

      def constraints do
        array_constraints()
        |> Keyword.take([
          :load,
          :create_action,
          :destroy_action,
          :update_action,
          :domain,
          :__source__
        ])
        |> Keyword.put(:on_update,
          type: {:one_of, [:update, :replace, :update_on_match]},
          default: :update_on_match,
          doc:
            "Whether or not setting the value should update the existing value, replace it, or update it if the primary key's match. If there is no primary key, `update_on_match` is the same as `replace`."
        )
      end

      def apply_constraints(nil, _), do: {:ok, nil}

      def apply_constraints(term, constraints) do
        Ash.load(term, constraints[:load] || [], lazy?: true, domain: ShadowDomain)
      end

      def handle_change(nil, new_value, _constraints) do
        {:ok, new_value}
      end

      def handle_change(old_value, nil, constraints) do
        action =
          constraints[:destroy_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :destroy).name

        old_value
        |> Ash.Changeset.new()
        |> Ash.EmbeddableType.copy_source(constraints)
        |> Ash.Changeset.for_destroy(action, %{}, domain: ShadowDomain)
        |> Ash.destroy()
        |> case do
          :ok -> {:ok, nil}
          {:error, error} -> {:error, Ash.EmbeddableType.handle_errors(error)}
        end
      end

      def handle_change(old_value, new_value, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        if Enum.empty?(pkey_fields) ||
             Enum.all?(pkey_fields, fn pkey_field ->
               !Ash.Resource.Info.attribute(__MODULE__, pkey_field).public?
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

            old_value
            |> Ash.Changeset.new()
            |> Ash.EmbeddableType.copy_source(constraints)
            |> Ash.Changeset.for_destroy(action, %{}, domain: ShadowDomain)
            |> Ash.destroy()
            |> case do
              :ok ->
                {:ok, new_value}

              {:error, error} ->
                {:error, Ash.EmbeddableType.handle_errors(error)}
            end
          end
        end
      end

      defp skip_unknown_inputs(constraints) do
        case Keyword.fetch(constraints, :__union_tag__) do
          {:ok, union_tag} when is_atom(union_tag) ->
            [union_tag, to_string(union_tag)]

          {:ok, union_tag} ->
            List.wrap(union_tag)

          _ ->
            []
        end
        |> Enum.concat(
          Enum.flat_map(Ash.Resource.Info.primary_key(__MODULE__), &[&1, to_string(&1)])
        )
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

        if Enum.empty?(pkey_fields) ||
             Enum.all?(pkey_fields, fn pkey_field ->
               !Ash.Resource.Info.attribute(__MODULE__, pkey_field).public?
             end) do
          action =
            constraints[:update_action] ||
              Ash.Resource.Info.primary_action!(__MODULE__, :update).name

          old_value
          |> Ash.Changeset.new()
          |> Ash.EmbeddableType.copy_source(constraints)
          |> Ash.Changeset.for_update(action, new_uncasted_value,
            domain: ShadowDomain,
            skip_unknown_inputs: skip_unknown_inputs(constraints)
          )
          |> Ash.update()
          |> case do
            {:ok, value} -> {:ok, value}
            {:error, error} -> {:error, Ash.EmbeddableType.handle_errors(error)}
          end
        else
          pkey =
            Enum.into(pkey_fields, %{}, fn pkey_field ->
              with {:ok, value} <- fetch_key(new_uncasted_value, pkey_field),
                   attribute <- Ash.Resource.Info.attribute(__MODULE__, pkey_field),
                   {:ok, casted} <-
                     Ash.Type.cast_input(attribute.type, value, attribute.constraints) do
                {pkey_field, casted}
              else
                _ -> {pkey_field, :error}
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
              |> Ash.EmbeddableType.copy_source(constraints)
              |> Ash.Changeset.for_update(action, new_uncasted_value,
                domain: ShadowDomain,
                skip_unknown_inputs: skip_unknown_inputs(constraints)
              )
              |> Ash.update()
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
      alias Ash.EmbeddableType.ShadowDomain

      def cast_atomic_array(value, constraints) do
        if Ash.Expr.expr?(value) do
          if Enum.empty?(Ash.Resource.Info.primary_key(__MODULE__)) ||
               constraints[:on_update] == :replace do
            case cast_input_array(value, constraints) do
              {:ok, value} ->
                {:atomic, value}

              {:error, error} ->
                {:error, error}
            end
          else
            {:not_atomic,
             "Embedded attributes do not support atomic updates unless they have no primary key, or `constraints[:on_update]` is set to `:replace`."}
          end
        else
          {:not_atomic,
           "Embedded attributes do not support atomic updates with expressions, only literal values."}
        end
      end

      def loaded?(record, path_to_load, _constraints, opts) do
        Ash.Resource.loaded?(record, path_to_load, opts)
      end

      def load(record, load, _constraints, %{domain: domain} = context) do
        opts = Ash.Context.to_opts(context, domain: domain)

        attribute_loads =
          __MODULE__ |> Ash.Resource.Info.public_attributes() |> Enum.map(& &1.name)

        load =
          case load do
            %Ash.Query{} ->
              Ash.Query.ensure_selected(load, attribute_loads)

            load_statement ->
              __MODULE__ |> Ash.Query.select(attribute_loads) |> Ash.Query.load(load_statement)
          end

        Ash.load(record, load, opts)
      end

      def merge_load(left, right, constraints, context) do
        right = Ash.Query.load(__MODULE__, right)

        __MODULE__
        |> Ash.Query.new()
        |> Ash.Query.load(left)
        |> case do
          %{valid?: true} = left ->
            {:ok, Ash.Query.merge_query_load(left, right, context)}

          query ->
            {:error, Ash.Error.to_ash_error(query.errors)}
        end
      end

      def get_rewrites(merged_load, calculation, path, _) do
        merged_load = Ash.Query.load(__MODULE__, merged_load)
        Ash.Actions.Read.Calculations.get_all_rewrites(merged_load, calculation, path)
      end

      def rewrite(value, rewrites, _constraints) do
        Ash.Actions.Read.Calculations.rewrite(rewrites, value)
      end

      def array_constraints, do: Ash.EmbeddableType.embedded_resource_array_constraints()

      def apply_constraints_array(term, constraints) do
        case find_duplicates(
               term,
               __MODULE__ |> Ash.Resource.Info.unique_keys()
             ) do
          nil ->
            if constraints[:sort] do
              query =
                __MODULE__
                |> Ash.DataLayer.Simple.set_data(term)
                |> Ash.EmbeddableType.copy_source(constraints)
                |> Ash.Query.load(constraints[:load] || [])

              query =
                if constraints[:sort] do
                  Ash.Query.sort(query, constraints[:sort])
                else
                  query
                end

              case Ash.read(query, domain: ShadowDomain) do
                {:ok, result} ->
                  case Ash.Type.list_constraint_errors(result, constraints) do
                    [] ->
                      {:ok, result}

                    errors ->
                      {:error, errors}
                  end
              end
            else
              if constraints[:load] do
                query =
                  __MODULE__
                  |> Ash.DataLayer.Simple.set_data(term)
                  |> Ash.EmbeddableType.copy_source(constraints)
                  |> Ash.Query.load(constraints[:load] || [])

                Ash.load(term, query, domain: ShadowDomain)
              else
                {:ok, term}
              end
            end

          keys ->
            {:error, message: "items must be unique on keys %{keys}", keys: Enum.join(keys, ",")}
        end
      end

      defp find_duplicates([], _), do: nil
      defp find_duplicates([_item], _), do: nil

      defp find_duplicates(list, unique_key_configs) do
        Enum.find(unique_key_configs, fn unique_key_config ->
          unique_key = unique_key_config[:keys]
          attributes = Enum.map(unique_key, &Ash.Resource.Info.attribute(__MODULE__, &1))

          if Enum.all?(attributes, &Ash.Type.simple_equality?(&1.type)) do
            list
            |> Enum.flat_map(fn item ->
              value =
                item
                |> Map.take(unique_key)
                |> Map.values()

              if unique_key_config.nils_distinct? && Enum.any?(value, &is_nil/1) do
                []
              else
                [value]
              end
            end)
            |> Enum.reduce_while({false, MapSet.new()}, fn item, {false, acc} ->
              if MapSet.member?(acc, item) do
                {:halt, {true, acc}}
              else
                {:cont, {false, MapSet.put(acc, item)}}
              end
            end)
            |> elem(0)
          else
            Enum.reduce_while(list, list, fn
              _term, [_] ->
                {:halt, false}

              this, [_ | rest] ->
                has_duplicate? =
                  Enum.any?(rest, fn other ->
                    Enum.all?(attributes, fn attribute ->
                      this_value = Map.get(this, attribute.name)
                      other_value = Map.get(other, attribute.name)

                      if unique_key_config.nils_distinct? do
                        not is_nil(this_value) and not is_nil(other_value) and
                          Ash.Type.equal?(attribute.type, this_value, other_value)
                      else
                        Ash.Type.equal?(attribute.type, this_value, other_value)
                      end
                    end)
                  end)

                if has_duplicate? do
                  {:halt, true}
                else
                  {:cont, rest}
                end
            end)
          end
        end)
      end

      def handle_change_array(nil, new_values, constraints) do
        handle_change_array([], new_values, constraints)
      end

      def handle_change_array(old_values, nil, constraints) do
        case handle_change_array(old_values, [], constraints) do
          {:ok, []} -> {:ok, nil}
          other -> other
        end
      end

      def handle_change_array(old_values, new_values, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        destroy_action =
          constraints[:destroy_action] ||
            Ash.Resource.Info.primary_action!(__MODULE__, :destroy).name

        old_values
        |> List.wrap()
        |> Enum.with_index()
        |> then(fn list ->
          if Enum.empty?(pkey_fields) do
            list
          else
            list
            |> Enum.reject(fn {old_value, _} ->
              pkey = Map.take(old_value, pkey_fields)

              Enum.any?(new_values, fn new_value ->
                Map.take(new_value, pkey_fields) == pkey
              end)
            end)
          end
        end)
        |> Enum.reduce_while(:ok, fn {record, index}, :ok ->
          record
          |> Ash.Changeset.new()
          |> Ash.EmbeddableType.copy_source(constraints)
          |> Ash.Changeset.for_destroy(destroy_action, %{}, domain: ShadowDomain)
          |> Ash.destroy()
          |> case do
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

      def include_source(constraints, changeset) do
        Keyword.put(constraints, :__source__, changeset)
      end

      def prepare_change_array(_old_values, nil, _constraints) do
        {:ok, nil}
      end

      def prepare_change_array(old_values, new_uncasted_values, constraints) do
        pkey_fields = Ash.Resource.Info.primary_key(__MODULE__)

        if Enum.empty?(pkey_fields) ||
             Enum.all?(pkey_fields, fn pkey_field ->
               !Ash.Resource.Info.attribute(__MODULE__, pkey_field).public?
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
                    old_values
                    |> List.wrap()
                    |> Enum.find(fn old_value ->
                      Map.take(old_value, pkey_fields) ==
                        pkey
                    end)

                  if value_updating_from do
                    value_updating_from
                    |> Ash.Changeset.new()
                    |> Ash.EmbeddableType.copy_source(constraints)
                    |> Ash.Changeset.for_update(action, new,
                      domain: ShadowDomain,
                      skip_unknown_inputs: skip_unknown_inputs(constraints)
                    )
                    |> Ash.update()
                    |> case do
                      {:ok, value} ->
                        {:cont, {:ok, [value | new_uncasted_values]}}

                      {:error, error} ->
                        errors =
                          error
                          |> Ash.EmbeddableType.handle_errors()
                          |> Enum.map(fn error ->
                            cond do
                              is_exception(error) ->
                                Ash.Error.set_path(error, [index])

                              Keyword.keyword?(error) ->
                                error
                                |> Keyword.put(:index, index)
                                |> Keyword.update(:path, [index], &[index | &1])

                              true ->
                                error
                            end
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

  defmacro define_embeddable_type(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      use Ash.Type, embedded?: true

      Ash.EmbeddableType.single_embed_implementation(opts)
      Ash.EmbeddableType.array_embed_implementation()
    end
  end

  def copy_source(changeset, opts) do
    changeset =
      if source = opts[:__source__] do
        changeset
        |> Ash.Changeset.set_tenant(source.tenant)
        |> Ash.Changeset.set_context(source.context)
        |> Ash.Changeset.set_context(%{__source__: source})
        |> Map.put(:domain, source.domain)
      else
        changeset
      end

    if domain = opts[:domain] do
      Map.put(changeset, :domain, domain)
    else
      changeset
    end
  end
end
