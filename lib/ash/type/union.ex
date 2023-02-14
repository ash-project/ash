defmodule Ash.Type.Union do
  @constraints [
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
              tag: :type,
              tag_value: "my_object"
            ],
            other_object: [
              type: MyOtherObjectType,
              tag: :type,
              tag_value: "my_other_object"
            ],
            other_object_without_type: [
              type: MyOtherObjectTypeWithoutType,
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

       if !Ash.Type.ash_type?(type) do
         raise """
         Unknown type in union type \"#{key}\": #{inspect(config[:type])}
         """
       end

       schema = Ash.Type.constraints(type)
       constraints = Spark.OptionsHelpers.validate!(config[:constraints] || [], schema)

       {key, Keyword.put(config, :constraints, constraints)}
     end)}
  end

  @moduledoc """
  A union between multiple types, distinguished with a tag or by attempting to validate.

  ## Constraints

  #{Spark.OptionsHelpers.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  ## Find the minimal supported type?
  @impl true
  def storage_type, do: :map

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(%Ash.Union{value: value, type: type_name}, constraints) do
    type = constraints[:types][type_name][:type]
    inner_constraints = constraints[:types][type_name][:constraints] || []

    case Ash.Type.cast_input(type, value, inner_constraints) do
      {:ok, value} ->
        {:ok, %Ash.Union{value: value, type: type_name}}

      error ->
        error
    end
  end

  def cast_input(value, constraints) do
    types = constraints[:types] || []

    types
    |> Enum.sort_by(fn {_type_name, config} -> config[:tag] end)
    |> Enum.reverse()
    |> Enum.reduce_while({:error, []}, fn {type_name, config}, {:error, errors} ->
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
                    Keyword.values(value)
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

        if (Map.get(value, config[:tag]) || Map.get(value, to_string(config[:tag]))) == tag_value do
          case Ash.Type.cast_input(type, value, config[:constraints] || []) do
            {:ok, value} ->
              case Ash.Type.apply_constraints(type, value, config[:constraints]) do
                {:ok, value} ->
                  {:halt,
                   {:ok,
                    %Ash.Union{
                      value: value,
                      type: type_name
                    }}}

                {:error, other} ->
                  {:halt, {:error, "is not a valid #{type_name}: #{inspect(other)}"}}
              end

            {:error, other} ->
              {:halt, {:error, "is not a valid #{type_name}: #{inspect(other)}"}}

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
              case Ash.Type.apply_constraints(type, value, config[:constraints]) do
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
    end)
    |> case do
      {:error, errors} when is_binary(errors) ->
        {:error, errors}

      {:error, errors} ->
        {:error, error_message(errors)}

      {:ok, value} ->
        {:ok, value}

      value ->
        value
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

  def cast_stored(%{"type" => type, "value" => value}, constraints) do
    type =
      if is_binary(type) do
        String.to_existing_atom(type)
      else
        type
      end

    types = constraints[:types] || []

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
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(%Ash.Union{value: value, type: type_name}, constraints) do
    type = constraints[:types][type_name][:type]
    constraints = constraints[:types][type_name][:constraints] || []

    if type do
      case Ash.Type.dump_to_native(type, value, constraints) do
        {:ok, value} ->
          {:ok, %{"type" => type_name, "value" => value}}

        other ->
          other
      end
    else
      :error
    end
  end
end
