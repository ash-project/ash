defmodule Ash.Type.Union do
  @constraints [
    storage: [
      type: {:one_of, [:type_and_value, :map_with_tag]},
      default: :type_and_value,
      doc: """
      How the value will be stored when persisted.

      `:type_and_value` will store the type and value in a map like so `{type: :type_name, value: the_value}`
      `:map_with_tag` will store the value directly. This only works if all types have a `tag` and `tag_value` configured.
      """
    ],
    types: [
      type: {:custom, __MODULE__, :union_types, []},
      doc: """
      The types to be unioned, a map of an identifier for the enum value to its configuration.

      When using `tag` and `tag_value` we are referring to a map key that must equal a certain value
      in order for the value to be considered an instance of that type.

      For example:

          types:  [
            int: [
              type: :integer,
              constraints: [
                max: 10
              ]
            ],
            object: [
              type: MyObjectType,
              # The default value is `true`
              # this passes the tag key/value to the nested type
              # when casting input
              cast_tag?: true,
              tag: :type,
              tag_value: "my_object"
            ],
            other_object: [
              type: MyOtherObjectType,
              cast_tag?: true,
              tag: :type,
              tag_value: "my_other_object"
            ],
            other_object_without_type: [
              type: MyOtherObjectTypeWithoutType,
              cast_tag?: false,
              tag: :type,
              tag_value: nil
            ]
          ]

      IMPORTANT:

      This is stored as a map under the hood. Filters over the data will need to take this into account.

      Additionally, if you are not using a tag, a value will be considered to be of the given type if it successfully casts.
      This means that, for example, if you try to cast `"10"` as a union of a string and an integer, it will end up as `"10"` because
      it is a string. If you put the integer type ahead of the string type, it will cast first and `10` will be the value.
      """
    ]
  ]

  @impl true
  def init(constraints) do
    constraints[:types]
    |> List.wrap()
    |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, types} ->
      type = config[:type]
      constraints = config[:constraints] || []

      if Keyword.get(config, :init?, true) do
        case Ash.Type.init(type, constraints) do
          {:ok, constraints} ->
            {:cont, {:ok, [{name, Keyword.put(config, :constraints, constraints)} | types]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      else
        {:cont, {:ok, [{name, config} | types]}}
      end
    end)
    |> case do
      {:ok, types} ->
        {:ok, Keyword.put(constraints, :types, Enum.reverse(types))}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def union_types(value) do
    {:ok,
     Enum.reduce(value, [], fn {name, config}, types ->
       config =
         Keyword.update!(config, :type, fn type ->
           Ash.Type.get_type(type)
         end)

       if config[:type] == Ash.Union do
         config
         |> Keyword.update!(:constraints, fn constraints ->
           Keyword.update!(constraints, :types, fn types ->
             {:ok, types} = union_types(types)
             types
           end)
         end)
         |> Keyword.get(:constraints)
         |> Keyword.get(:types)
         |> Enum.reduce(types, fn {new_name, new_config}, types ->
           if types[new_name] do
             raise "Detected a conflict in nested union type names. They must be unique all the way down."
           else
             Keyword.put(types, new_name, new_config)
           end
         end)
       else
         Keyword.put(types, name, config)
       end
     end)
     |> Keyword.new(fn {key, config} ->
       type = Ash.Type.get_type(config[:type])
       Code.ensure_compiled!(unwrap_type(type))

       if !Ash.Type.ash_type?(type) do
         raise """
         Unknown type in union type \"#{key}\": #{inspect(config[:type])}
         """
       end

       schema = Ash.Type.constraints(type)
       constraints = Spark.Options.validate!(config[:constraints] || [], schema)

       {key, config |> Keyword.put(:constraints, constraints) |> Keyword.put(:type, type)}
     end)}
  end

  defp unwrap_type({:array, type}), do: unwrap_type(type)
  defp unwrap_type(type), do: type

  @moduledoc """
  A union between multiple types, distinguished with a tag or by attempting to validate.

  ## Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def matches_type?(%Ash.Union{type: type, value: value}, constraints) do
    Ash.Type.matches_type?(constraints[:types][type], value)
  end

  ## Find the minimal supported type?
  @impl true
  def storage_type(_), do: :map

  def loaded?(%Ash.Union{type: type, value: value}, path_to_load, constraints, opts) do
    config = constraints[:types][type]

    if opts[:type] == :request do
      case path_to_load do
        [:* | rest] ->
          Ash.Type.loaded?(config[:type], value, rest, config[:constraints], opts)

        [^type | rest] ->
          Ash.Type.loaded?(config[:type], value, rest, config[:constraints], opts)

        [first | _rest] ->
          if Enum.any?(constraints[:types], fn {key, _} -> key == first end) do
            true
          else
            false
          end

        [] ->
          true
      end
    else
      case path_to_load do
        [:value | rest] ->
          Ash.Type.loaded?(config[:type], value, rest, config[:constraints], opts)

        [] ->
          true

        _ ->
          false
      end
    end
  end

  @impl true
  def load(unions, [], _, _), do: {:ok, unions}

  @impl true
  def load(unions, load, constraints, context) do
    if Enum.any?(constraints[:types], fn {_name, config} ->
         Ash.Type.can_load?(config[:type], config[:constraints])
       end) do
      unions
      |> Stream.with_index()
      |> Stream.map(fn {item, index} ->
        Map.put(item, :__index__, index)
      end)
      |> Enum.group_by(& &1.type)
      |> Enum.reduce_while({:ok, []}, fn {name, values}, {:ok, acc} ->
        value_indexes_to_full_index =
          values
          |> Enum.with_index()
          |> Map.new(fn {value, index} ->
            {index, value.__index__}
          end)

        values = Enum.map(values, & &1.value)

        our_load =
          if load[:*] || load[name] do
            List.wrap(load[:*]) ++ List.wrap(load[name])
          end

        result =
          if our_load do
            type = constraints[:types][name][:type]

            Ash.Type.load(
              type,
              values,
              our_load,
              constraints[:types][name][:constraints],
              context
            )
          else
            type = constraints[:types][name][:type]
            constraints = constraints[:types][name][:constraints]

            if Ash.Type.can_load?(type, constraints) do
              Ash.Type.load(
                type,
                values,
                [],
                constraints,
                context
              )
            else
              {:ok, values}
            end
          end

        case result do
          {:ok, values} ->
            values_with_index =
              values
              |> Enum.with_index()
              |> Enum.map(fn {value, index} ->
                Map.put(
                  %Ash.Union{value: value, type: name},
                  :__index__,
                  Map.get(value_indexes_to_full_index, index)
                )
              end)

            {:cont, {:ok, [values_with_index | acc]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, batches} ->
          {:ok,
           batches
           |> Stream.flat_map(& &1)
           |> Enum.sort_by(& &1.__index__)
           |> Enum.map(&Map.delete(&1, :__index__))}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, unions}
    end
  end

  @impl Ash.Type
  def merge_load(left, right, constraints, context) do
    constraints[:types]
    |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, acc} ->
      merged =
        [left[name], right[name], left[:*], right[:*]]
        |> Enum.reject(&is_nil/1)
        |> case do
          [] ->
            {:ok, []}

          load_statements ->
            Enum.reduce_while(load_statements, {:ok, []}, fn load_statement, {:ok, merged} ->
              if merged in [nil, [], %{}] do
                {:cont, {:ok, load_statement}}
              else
                case Ash.Type.merge_load(
                       config[:type],
                       merged,
                       load_statement,
                       config[:constraints],
                       context
                     ) do
                  {:ok, merged} -> {:cont, {:ok, merged}}
                  {:error, error} -> {:halt, {:error, error}}
                end
              end
            end)
        end

      case merged do
        {:ok, empty} when empty in [nil, []] ->
          {:cont, {:ok, acc}}

        {:ok, merged} ->
          {:cont, {:ok, Keyword.put(acc, name, merged)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  @impl Ash.Type
  def get_rewrites(merged_load, calculation, path, constraints) do
    merged_load
    |> Enum.flat_map(fn {key, type_load} ->
      if Ash.Type.can_load?(
           constraints[:types][key][:type],
           constraints[:types][key][:constraints] || []
         ) do
        constraints[:types][key][:type]
        |> Ash.Type.get_rewrites(
          type_load,
          calculation,
          path,
          constraints[:types][key][:constraints] || []
        )
        |> Enum.map(fn {{rewrite_path, data, name, load}, source} ->
          {{[key | rewrite_path], data, name, load}, source}
        end)
      else
        []
      end
    end)
  end

  @impl Ash.Type
  def rewrite(value, rewrites, constraints) do
    type_rewrites =
      Enum.flat_map(rewrites, fn
        {:cleanup_field_auth, further} ->
          [{:cleanup_field_auth, further[value.type] || []}]

        {{[first | path_rest], data, name, load}, source} ->
          if first == value.type do
            [{{path_rest, data, name, load}, source}]
          else
            []
          end

        _ ->
          []
      end)

    Map.update!(
      value,
      :value,
      &Ash.Type.rewrite(
        constraints[:types][value.type][:type],
        &1,
        type_rewrites,
        constraints[:types][value.type][:type]
      )
    )
  end

  @impl true
  def cast_atomic(_new_value, _constraints) do
    {:not_atomic, "Unions do not support atomic updates"}
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(%Ash.Union{value: value, type: type_name}, constraints) do
    case try_cast_type(value, constraints, type_name, constraints[:types][type_name], [], false) do
      {_, {:ok, value}} ->
        {:ok, value}

      {_, {:expose_error, errors}} ->
        {:error, errors}

      {_, {:error, error}} ->
        {:error, List.flatten(Keyword.values(error))}
    end
  end

  def cast_input(%{"_union_type" => union_type} = value, constraints) do
    case Enum.find(constraints[:types], fn {key, _} ->
           to_string(key) == union_type
         end) do
      {type, _config} ->
        cast_input(%Ash.Union{value: Map.delete(value, "_union_type"), type: type}, constraints)

      _ ->
        cast_input(Map.delete(value, "_union_type"), constraints)
    end
  end

  def cast_input(value, constraints) do
    types = constraints[:types] || []

    types
    |> Enum.reduce_while({:error, []}, fn {type_name, config}, {:error, errors} ->
      try_cast_type(value, constraints, type_name, config, errors)
    end)
    |> case do
      {:error, errors} when is_binary(errors) ->
        {:error, errors}

      {:error, errors} ->
        {:error, error_message(errors)}

      {:expose_error, errors} ->
        {:error, errors}

      {:ok, value} ->
        {:ok, value}

      value ->
        value
    end
  end

  defp try_cast_type(value, constraints, type_name, config, errors, tags_must_match? \\ true) do
    type = config[:type]

    if is_map(value) && config[:tag] do
      tag_value = config[:tag_value]

      value =
        if Ash.Type.embedded_type?(type) && !is_struct(value) do
          Enum.reduce(Ash.Resource.Info.attributes(type), value, fn attr, value ->
            with {:array, _nested_type} <- attr.type,
                 true <- has_key?(value, attr.name) do
              update_key(value, attr.name, fn value ->
                if is_map(value) && !is_struct(value) do
                  Map.values(value)
                else
                  value
                end
              end)
            else
              _ ->
                value
            end
          end)
        else
          value
        end

      if !tags_must_match? || tags_equal?(tag_value, get_tag(value, config[:tag])) do
        value =
          if Keyword.get(config, :cast_tag?, true) do
            value
          else
            Map.drop(value, [config[:tag], to_string(config[:tag])])
          end

        config_constraints =
          if Ash.Type.embedded_type?(config[:type]) do
            Keyword.put(config[:constraints] || [], :__union_tag__, config[:tag])
          else
            config[:constraints] || []
          end

        constraints_with_source =
          Ash.Type.include_source(
            config[:type],
            constraints[:__source__],
            config_constraints
          )

        case Ash.Type.cast_input(
               type,
               value,
               constraints_with_source
             ) do
          {:ok, value} ->
            case Ash.Type.apply_constraints(type, value, constraints_with_source) do
              {:ok, value} ->
                {:halt,
                 {:ok,
                  %Ash.Union{
                    value: value,
                    type: type_name
                  }}}

              {:error, other} ->
                {:halt, {:expose_error, other}}
            end

          {:error, other} ->
            {:halt, {:expose_error, other}}

          :error ->
            {:halt, {:error, "is not a valid #{type_name}"}}
        end
      else
        {:cont,
         {:error,
          Keyword.put(errors, type_name, "#{config[:tag]} does not equal #{config[:tag_value]}")}}
      end
    else
      if config[:tag] do
        {:cont, {:error, Keyword.put(errors, type_name, "is not a map")}}
      else
        case Ash.Type.cast_input(type, value, config[:constraints] || []) do
          {:ok, value} ->
            case Ash.Type.apply_constraints(type, value, config[:constraints] || []) do
              {:ok, value} ->
                {:halt,
                 {:ok,
                  %Ash.Union{
                    value: value,
                    type: type_name
                  }}}

              {:error, other} ->
                {:cont, {:error, Keyword.put(errors, type_name, other)}}
            end

          {:error, other} ->
            {:cont, {:error, Keyword.put(errors, type_name, other)}}

          :error ->
            {:cont, {:error, Keyword.put(errors, type_name, "is invalid")}}
        end
      end
    end
  end

  defp get_tag(map, tag) do
    Map.get(map, tag, Map.get(map, to_string(tag)))
  end

  defp tags_equal?(tag_value, their_tag_value) do
    cond do
      is_atom(tag_value) ->
        their_tag_value == tag_value || their_tag_value == to_string(tag_value)

      is_binary(tag_value) && is_atom(their_tag_value) ->
        their_tag_value == tag_value || to_string(their_tag_value) == tag_value

      true ->
        their_tag_value == tag_value
    end
  end

  defp has_key?(map, key) do
    Map.has_key?(map, key) || Map.has_key?(map, to_string(key))
  end

  defp update_key(map, key, func) do
    cond do
      Map.has_key?(map, key) ->
        Map.update!(map, key, func)

      Map.has_key?(map, to_string(key)) ->
        Map.update!(map, to_string(key), func)

      true ->
        map
    end
  end

  defp error_message(errors) do
    "No union type matched\n" <>
      Enum.map_join(errors, "\n", fn {key, errors} ->
        "  #{key}: #{inspect(errors)}"
      end)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_map(value) do
    types = constraints[:types] || []

    case constraints[:storage] do
      :type_and_value ->
        case value do
          %{"type" => type, "value" => value} ->
            type =
              if is_binary(type) do
                String.to_existing_atom(type)
              else
                type
              end

            case Keyword.fetch(types, type) do
              {:ok, config} ->
                case Ash.Type.cast_stored(config[:type], value, config[:constraints]) do
                  {:ok, casted_value} ->
                    {:ok,
                     %Ash.Union{
                       value: casted_value,
                       type: type
                     }}

                  other ->
                    other
                end

              other ->
                other
            end

          _ ->
            :error
        end

      :map_with_tag ->
        case Enum.find(types, fn {_type_name, config} ->
               unless config[:tag] && config[:tag_value] do
                 raise "Found a type without a tag when using the `:map_with_tag` storage constraint. Constraints: #{inspect(constraints)}"
               end

               their_tag_value = get_tag(value, config[:tag])

               tags_equal?(config[:tag_value], their_tag_value)
             end) do
          nil ->
            :error

          {type_name, config} ->
            case Ash.Type.cast_stored(config[:type], value, config[:constraints]) do
              {:ok, casted_value} ->
                {:ok,
                 %Ash.Union{
                   value: casted_value,
                   type: type_name
                 }}

              other ->
                other
            end
        end
    end
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(%Ash.Union{value: value, type: type_name}, union_constraints) do
    type = union_constraints[:types][type_name][:type]

    if type do
      constraints = union_constraints[:types][type_name][:constraints] || []

      case union_constraints[:storage] do
        :type_and_value ->
          case Ash.Type.dump_to_native(type, value, constraints) do
            {:ok, value} ->
              {:ok, %{"type" => type_name, "value" => value}}

            other ->
              other
          end

        :map_with_tag ->
          config = union_constraints[:types][type_name]

          unless config[:tag] && config[:tag_value] do
            raise "Found a type without a tag when using the `:map_with_tag` storage constraint. Constraints: #{inspect(union_constraints)}"
          end

          Ash.Type.dump_to_native(type, value, constraints)
      end
    else
      :error
    end
  end

  @impl true
  def handle_change(nil, %Ash.Union{type: type_name, value: new_value}, constraints),
    do: do_handle_change(type_name, nil, new_value, constraints)

  def handle_change(%Ash.Union{type: type_name, value: old_value}, nil, constraints),
    do: do_handle_change(type_name, old_value, nil, constraints)

  def handle_change(
        %Ash.Union{type: type_name, value: old_value},
        %Ash.Union{type: type_name, value: new_value},
        constraints
      ),
      do: do_handle_change(type_name, old_value, new_value, constraints)

  def handle_change(
        %Ash.Union{},
        %Ash.Union{type: type_name, value: new_value},
        constraints
      ),
      do: do_handle_change(type_name, nil, new_value, constraints)

  defp do_handle_change(type_name, old_value, new_value, constraints) do
    with {:ok, type_configs} <- Keyword.fetch(constraints, :types),
         {:ok, type_config} <- Keyword.fetch(type_configs, type_name),
         {:ok, type} <- Keyword.fetch(type_config, :type),
         type_constraints <- Keyword.get(type_config, :constraints, []),
         type <- Ash.Type.get_type(type),
         {:ok, new_value} <- Ash.Type.handle_change(type, old_value, new_value, type_constraints) do
      {:ok, %Ash.Union{type: type_name, value: new_value}}
    end
  end

  @impl true
  def prepare_change_array(
        old_values,
        new_values,
        constraints
      ) do
    if Enum.any?(constraints[:types] || [], fn {_name, config} ->
         Ash.Type.prepare_change_array?(config[:type])
       end) do
      old_values_by_type =
        old_values
        |> List.wrap()
        |> Stream.with_index()
        |> Stream.map(fn {item, index} ->
          Map.put(item, :__index__, index)
        end)
        |> Enum.group_by(& &1.type, & &1.value)

      new_values
      |> Stream.with_index()
      |> Stream.map(fn {item, index} ->
        if is_map(item) do
          Map.put(item, :__index__, index)
        else
          {:untagged, item, index}
        end
      end)
      |> Enum.group_by(
        fn
          {:untagged, _item, _index} ->
            :__ash_untagged_unions__

          %Ash.Union{type: type} ->
            type

          item ->
            Enum.find_value(
              constraints[:types] || [],
              :__ash_untagged_unions__,
              fn {name, config} ->
                field = config[:tag]
                tag = config[:tag_value]

                if field && tag && tags_equal?(get_tag(item, field), tag) do
                  name
                end
              end
            )
        end,
        fn
          %Ash.Union{value: value} = union ->
            {:union_value, value, Map.get(union, :__index__)}

          other ->
            other
        end
      )
      |> Enum.reduce_while({:ok, []}, fn
        {:__ash_untagged_unions__, values}, {:ok, acc} ->
          {:cont, {:ok, [values | acc]}}

        {name, new_values}, {:ok, acc} ->
          value_indexes_to_full_index =
            new_values
            |> Enum.with_index()
            |> Map.new(fn
              {{:union_value, _value, value_index}, index} ->
                {index, value_index}

              {value, index} ->
                {index, value.__index__}
            end)

          new_values =
            Enum.map(new_values, fn
              {:union_value, value, _} ->
                case value do
                  %{__index__: _} = value ->
                    Map.delete(value, :__index__)

                  value ->
                    value
                end

              value ->
                case value do
                  %{__index__: _} = value ->
                    Map.delete(value, :__index__)

                  value ->
                    value
                end
            end)

          type = constraints[:types][name][:type]
          type_constraints = constraints[:types][name][:constraints] || []

          type_constraints =
            if union_tag = constraints[:types][name][:tag] do
              Keyword.put(type_constraints, :__union_tag__, union_tag)
            else
              type_constraints
            end

          item_constraints =
            Ash.Type.include_source({:array, type}, constraints[:__source__],
              items: type_constraints
            )

          result =
            Ash.Type.prepare_change(
              {:array, type},
              old_values_by_type[name] || [],
              new_values,
              item_constraints
            )

          case result do
            {:ok, values} ->
              values_with_index =
                values
                |> Enum.with_index()
                |> Enum.map(fn {value, index} ->
                  Map.put(
                    %Ash.Union{value: value, type: name},
                    :__index__,
                    Map.get(value_indexes_to_full_index, index)
                  )
                end)

              {:cont, {:ok, [values_with_index | acc]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
      end)
      |> case do
        {:ok, batches} ->
          {:ok,
           batches
           |> Stream.flat_map(& &1)
           |> Enum.sort_by(fn
             {:untagged, _item, index} ->
               index

             item ->
               item.__index__
           end)
           |> Enum.map(fn
             {:untagged, item, _} ->
               item

             item ->
               Map.delete(item, :__index__)
           end)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, new_values}
    end
  end

  @impl true
  def include_source(constraints, changeset) do
    Keyword.put(constraints || [], :__source__, changeset)
  end

  @impl true
  def handle_change_array(
        old_values,
        new_values,
        constraints
      ) do
    if Enum.any?(constraints[:types] || [], fn {_name, config} ->
         Ash.Type.handle_change_array?(config[:type])
       end) do
      old_values_by_type =
        old_values
        |> List.wrap()
        |> Stream.with_index()
        |> Stream.map(fn {item, index} ->
          Map.put(item, :__index__, index)
        end)
        |> Enum.group_by(& &1.type, & &1.value)

      new_values
      |> Stream.with_index()
      |> Stream.map(fn {item, index} ->
        Map.put(item, :__index__, index)
      end)
      |> Enum.group_by(& &1.type)
      |> Enum.reduce_while({:ok, []}, fn {name, new_values}, {:ok, acc} ->
        value_indexes_to_full_index =
          new_values
          |> Enum.with_index()
          |> Map.new(fn {value, index} ->
            {index, value.__index__}
          end)

        new_values = Enum.map(new_values, & &1.value)

        type = constraints[:types][name][:type]
        type_constraints = constraints[:types][name][:constraints] || []

        type_constraints =
          if union_tag = constraints[:types][name][:tag] do
            Keyword.put(type_constraints, :__union_tag__, union_tag)
          else
            type_constraints
          end

        result =
          Ash.Type.handle_change(
            {:array, type},
            old_values_by_type[name] || [],
            new_values,
            Keyword.put(constraints, :items, type_constraints)
          )

        case result do
          {:ok, values} ->
            values_with_index =
              values
              |> Enum.with_index()
              |> Enum.map(fn {value, index} ->
                Map.put(
                  %Ash.Union{value: value, type: name},
                  :__index__,
                  Map.get(value_indexes_to_full_index, index)
                )
              end)

            {:cont, {:ok, [values_with_index | acc]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:ok, batches} ->
          {:ok,
           batches
           |> Stream.flat_map(& &1)
           |> Enum.sort_by(& &1.__index__)
           |> Enum.map(&Map.delete(&1, :__index__))}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, new_values}
    end
  end

  @impl true
  def prepare_change(_old_value, nil, _constraints), do: {:ok, nil}

  def prepare_change(nil, new_value, constraints) do
    case cast_input(new_value, constraints) do
      {:ok, %Ash.Union{type: type_name, value: value}} ->
        do_prepare_change(type_name, nil, value, constraints)

      {:error, _} ->
        {:ok, new_value}
    end
  end

  def prepare_change(
        %Ash.Union{type: type_name, value: old_value},
        %Ash.Union{type: type_name, value: new_value},
        constraints
      ) do
    with {:ok, type_configs} <- Keyword.fetch(constraints, :types),
         {:ok, type_config} <- Keyword.fetch(type_configs, type_name),
         {:ok, type} <- Keyword.fetch(type_config, :type),
         type_constraints <- Keyword.get(type_config, :constraints, []),
         type <- Ash.Type.get_type(type),
         {:ok, value} <- type.prepare_change(old_value, new_value, type_constraints) do
      {:ok, %Ash.Union{type: type_name, value: value}}
    end
  end

  def prepare_change(
        %Ash.Union{},
        %Ash.Union{type: type_name, value: new_value},
        constraints
      ) do
    with {:ok, type_configs} <- Keyword.fetch(constraints, :types),
         {:ok, type_config} <- Keyword.fetch(type_configs, type_name),
         {:ok, type} <- Keyword.fetch(type_config, :type),
         type_constraints <- Keyword.get(type_config, :constraints, []),
         type <- Ash.Type.get_type(type),
         {:ok, value} <- type.prepare_change(nil, new_value, type_constraints) do
      {:ok, %Ash.Union{type: type_name, value: value}}
    end
  end

  def prepare_change(%Ash.Union{type: type_name, value: old_value}, new_value, constraints)
      when is_map(new_value) do
    constraints
    |> Keyword.get(:types, [])
    |> Keyword.get(type_name, [])
    |> Map.new()
    |> case do
      %{tag: field, tag_value: tag} ->
        if tags_equal?(get_tag(new_value, field), tag),
          do: do_prepare_change(type_name, old_value, new_value, constraints),
          else: {:ok, new_value}

      _ ->
        {:ok, new_value}
    end
  end

  def prepare_change(%Ash.Union{type: type_name, value: old_value}, new_value, constraints) do
    case cast_input(new_value, constraints) do
      {:ok, %Ash.Union{type: ^type_name, value: value}} ->
        do_prepare_change(type_name, old_value, value, constraints)

      {:ok, %Ash.Union{type: other_type, value: value}} ->
        do_prepare_change(other_type, nil, value, constraints)

      {:error, _} ->
        {:ok, new_value}
    end
  end

  @impl true
  def can_load?(constraints) do
    Enum.any?(constraints[:types], fn {_key, config} ->
      Ash.Type.can_load?(config[:type], config[:constraints])
    end)
  end

  defp do_prepare_change(type_name, old_value, new_value, constraints) do
    with {:ok, type_configs} <- Keyword.fetch(constraints, :types),
         {:ok, type_config} <- Keyword.fetch(type_configs, type_name),
         {:ok, type} <- Keyword.fetch(type_config, :type),
         type_constraints <- Keyword.get(type_config, :constraints, []),
         type_constraints <-
           Keyword.put(type_constraints, :__union_tag__, type_config[:tag]),
         type <- Ash.Type.get_type(type),
         {:ok, value} <- type.prepare_change(old_value, new_value, type_constraints) do
      {:ok, %Ash.Union{type: type_name, value: value}}
    end
  end
end
