defmodule Ash.Type.Map do
  @constraints [
    fields: [
      type: :keyword_list,
      keys: [
        *: [
          type: :keyword_list,
          keys: [
            type: [
              type: Ash.OptionsHelpers.ash_type(),
              required: true
            ],
            allow_nil?: [
              type: :boolean,
              default: true
            ],
            constraints: [
              type: :keyword_list,
              default: []
            ]
          ]
        ]
      ],
      doc: """
      The types of the fields in the map, and their constraints.

      If constraints are specified, only those fields will be in the casted map.

      For example:

          fields:  [
            amount: [
              type: :integer,
              constraints: [
                max: 10
              ]
            ],
            currency: [
              type: :string,
              allow_nil?: false,
              constraints: [
                max_length: 3
              ]
            ]
          ]

      allow_nil? is true by default
      """
    ]
  ]

  @moduledoc """
  Represents a map stored in the database.

  In postgres, for example, this represents binary encoded json

  A builtin type that can be referenced via `:map`

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def init(constraints) do
    if is_list(constraints[:fields]) do
      constraints[:fields]
      |> List.wrap()
      |> Enum.reduce_while({:ok, []}, fn {name, config}, {:ok, fields} ->
        type = Ash.Type.get_type(config[:type])
        constraints = config[:constraints] || []

        if Keyword.get(config, :init?, true) do
          case Ash.Type.init(type, constraints) do
            {:ok, constraints} ->
              {:cont,
               {:ok,
                [{name, Keyword.merge(config, constraints: constraints, type: type)} | fields]}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        else
          {:cont, {:ok, [{name, config} | fields]}}
        end
      end)
      |> case do
        {:ok, fields} ->
          {:ok, Keyword.put(constraints, :fields, Enum.reverse(fields))}

        {:error, error} ->
          {:error, error}
      end
    else
      if is_nil(constraints[:fields]) do
        {:ok, constraints}
      else
        {:error, "fields must be a list, got `#{constraints[:fields]}`"}
      end
    end
  end

  @impl true
  def matches_type?(v, _constraints) do
    is_map(v)
  end

  @impl true
  def cast_input("", _), do: {:ok, nil}

  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_binary(value) do
    case Ash.Helpers.json_module().decode(value) do
      {:ok, value} ->
        cast_input(value, constraints)

      _ ->
        :error
    end
  end

  def cast_input(value, _) when is_map(value), do: {:ok, value}
  def cast_input(_, _), do: :error

  @impl true

  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_map(value) do
    if fields = constraints[:fields] do
      nil_values = constraints[:store_nil_values?]

      Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
        case fetch_field(value, key) do
          {:ok, value} ->
            case Ash.Type.cast_stored(config[:type], value, config[:constraints] || []) do
              {:ok, value} ->
                if is_nil(value) && !nil_values do
                  {:cont, {:ok, acc}}
                else
                  {:cont, {:ok, Map.put(acc, key, value)}}
                end

              other ->
                {:halt, other}
            end

          :error ->
            {:cont, {:ok, acc}}
        end
      end)
    else
      {:ok, value}
    end
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_map(value), do: {:ok, value}
  def dump_to_native(_, _), do: :error

  @impl true
  def apply_constraints(value, constraints) do
    Enum.reduce(constraints, {:ok, value}, fn
      {:fields, fields}, {:ok, value} ->
        check_fields(value, fields)

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  @impl true
  def generator(constraints) do
    if constraints[:fields] do
      optional =
        constraints[:fields]
        |> Enum.filter(fn {_, value} ->
          value[:allow_nil?]
        end)
        |> Keyword.keys()

      constraints[:fields]
      |> Map.new(fn {key, config} ->
        type = config[:type]
        constraints = config[:constraints] || []
        {key, Ash.Type.generator(type, constraints)}
      end)
      |> Ash.Generator.mixed_map(optional)
    else
      StreamData.constant(%{})
    end
  end

  defp check_fields(value, fields) do
    Enum.reduce(fields, {:ok, %{}}, fn
      {field, field_constraints}, {:ok, checked_value} ->
        case fetch_field(value, field) do
          {:ok, field_value} ->
            check_field(checked_value, field, field_value, field_constraints)

          :error ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "field must be present", field: field]]}
            else
              {:ok, checked_value}
            end
        end

      {_, _}, {:error, errors} ->
        {:error, errors}
    end)
  end

  defp check_field(result, field, field_value, field_constraints) do
    case Ash.Type.cast_input(
           field_constraints[:type],
           field_value,
           field_constraints[:constraints] || []
         ) do
      {:ok, field_value} ->
        case Ash.Type.apply_constraints(
               field_constraints[:type],
               field_value,
               field_constraints[:constraints] || []
             ) do
          {:ok, nil} ->
            if field_constraints[:allow_nil?] == false do
              {:error, [[message: "value must not be nil", field: field]]}
            else
              {:ok, Map.put(result, field, nil)}
            end

          {:ok, field_value} ->
            {:ok, Map.put(result, field, field_value)}

          {:error, errors} ->
            errors =
              if Keyword.keyword?(errors) do
                [errors]
              else
                List.wrap(errors)
              end

            {:error, Enum.map(errors, fn error -> Keyword.put(error, :field, field) end)}
        end

      {:error, error} ->
        {:error, [error]}

      :error ->
        {:error, [[message: "invalid value", field: field]]}
    end
  end

  defp fetch_field(map, atom) when is_atom(atom) do
    case Map.fetch(map, atom) do
      {:ok, value} -> {:ok, value}
      :error -> fetch_field(map, to_string(atom))
    end
  end

  defp fetch_field(map, key), do: Map.fetch(map, key)
end
