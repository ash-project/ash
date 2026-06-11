# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Struct do
  @constraints [
    instance_of: [
      type: :atom,
      doc: "The module the struct should be an instance of"
    ],
    preserve_nil_values?: [
      type: :boolean,
      default: false,
      doc: """
      If set to true, nil values will be preserved both when storing and in the casted struct.
      Otherwise, keys whose values are `nil` will be omitted.

      preserved_nil_values? is false by default
      """
    ],
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
            description: [
              type: :string
            ],
            constraints: [
              type: :keyword_list,
              default: []
            ],
            init?: [
              type: :boolean,
              default: true,
              doc: """
              If false, the field's type constraints are not initialised at compile time. \
              Allows for recursive struct fields.
              """
            ]
          ]
        ]
      ],
      doc: """
      The types of the fields in the struct, and their constraints.

      For example:

          fields:  [
            amount: [
              type: :integer,
              description: "The amount of the transaction",
              constraints: [
                max: 10
              ]
            ],
            currency: [
              type: :string,
              allow_nil?: false,
              description: "The currency code of the transaction",
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
  Represents a struct.

  Use the `instance_of` constraint to specify that it must be an instance of a specific struct.

  This cannot be loaded from a database unless the `instance_of` constraint is provided.
  If not, it can only be used to cast input, i.e for arguments.

  ## Alternative: Ash.TypedStruct

  For simpler use cases where you want to define a struct with typed fields inline,
  consider using `Ash.TypedStruct`. It provides a DSL for defining structs with:

  - Field type specifications and constraints
  - Default values
  - Required fields (via `allow_nil?: false`)
  - Automatic `new/1` and `new!/1` functions

  Example:

      defmodule MyStruct do
        use Ash.TypedStruct
        typed_struct do
          field :name, :string, allow_nil?: false
          field :age, :integer, constraints: [min: 0]
          field :email, :string, default: nil
        end
      end

  `Ash.TypedStruct` automatically creates an `Ash.Type.Struct` with the appropriate
  constraints under the hood.

  ## Constraints

  #{Spark.Options.docs(@constraints)}
  """

  use Ash.Type
  import Ash.Gettext

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :map

  @impl true
  def referenced_types(constraints), do: Ash.Type.field_referenced_types(constraints[:fields])

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

  defp fields(constraints) do
    case Keyword.fetch(constraints, :fields) do
      {:ok, fields} ->
        fields

      :error ->
        instance_of = constraints[:instance_of]

        if instance_of && Ash.Resource.Info.resource?(instance_of) do
          instance_of
          |> Ash.Resource.Info.attributes()
          |> Map.new(
            &{&1.name, [type: &1.type, constraints: &1.constraints, allow_nil?: &1.allow_nil?]}
          )
        end
    end
  end

  @impl true
  def matches_type?(v, constraints) do
    if instance_of = constraints[:instance_of] do
      is_struct(v, instance_of)
    else
      is_struct(v)
    end
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

  # stored fields are in the embedded format (see `dump_to_native/2`),
  # so they are loaded with `cast_from_embedded`
  def cast_stored(value, constraints) when is_map(value) do
    cast_struct_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  # fields are dumped with `dump_to_embedded` because the dumped map is
  # JSON-encoded by data layers, and `dump_to_native` may produce values
  # that are not JSON-safe (e.g. raw binaries for `:binary`/`:uuid_v7`)
  def dump_to_native(value, constraints) when is_map(value) do
    dump_struct_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_native(_, _), do: :error

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, constraints) when is_map(value) do
    dump_struct_fields(value, constraints, &Ash.Type.dump_to_embedded/3)
  end

  def dump_to_embedded(_, _), do: :error

  @impl true
  def cast_from_embedded(nil, _), do: {:ok, nil}

  def cast_from_embedded(value, constraints) when is_map(value) do
    cast_struct_fields(value, constraints, &Ash.Type.cast_from_embedded/3)
  end

  def cast_from_embedded(_, _), do: :error

  defp dump_struct_fields(value, constraints, dump_fn) do
    if fields = fields(constraints) do
      if constraints[:instance_of] do
        Enum.reduce_while(fields, {:ok, %{}}, fn {key, config}, {:ok, acc} ->
          case dump_struct_field(value, key, config, dump_fn) do
            {:ok, dumped} -> {:cont, {:ok, Map.put(acc, key, dumped)}}
            :skip -> {:cont, {:ok, acc}}
            other -> {:halt, other}
          end
        end)
      else
        :error
      end
    else
      :error
    end
  end

  defp dump_struct_field(value, key, config, dump_fn) do
    case Map.fetch(value, key) do
      {:ok, field_value} -> dump_fn.(config[:type], field_value, config[:constraints] || [])
      :error -> :skip
    end
  end

  defp cast_struct_fields(value, constraints, cast_fn) do
    if fields = fields(constraints) do
      if constraints[:instance_of] do
        nil_values = constraints[:preserve_nil_values?]

        Enum.reduce_while(fields, {:ok, struct(constraints[:instance_of], [])}, fn {key, config},
                                                                                   {:ok, acc} ->
          case fetch_field(value, key) do
            {:ok, value} ->
              case cast_fn.(config[:type], value, config[:constraints] || []) do
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
        :error
      end
    else
      :error
    end
  end

  @impl true
  def generator(constraints) do
    if !constraints[:instance_of] do
      raise ArgumentError,
            "Cannot generate instances of the `:struct` type without an `:instance_of` constraint"
    end

    Ash.Type.Map.generator(constraints)
    |> StreamData.map(fn value ->
      struct(constraints[:instance_of], value)
    end)
  end

  @impl true
  def apply_constraints(nil, _constraints), do: {:ok, nil}

  def apply_constraints(value, constraints) do
    instance_of = constraints[:instance_of]

    if instance_of && !constraints[:fields] && is_struct(value, instance_of) do
      {:ok, value}
    else
      with {:ok, value} <- handle_fields(value, constraints) do
        handle_instance_of(value, constraints)
      end
    end
  end

  @impl Ash.Type
  def load(record, load, constraints, %{domain: domain} = context) do
    instance_of = constraints[:instance_of]
    opts = Ash.Context.to_opts(context, domain: domain)

    cond do
      is_nil(instance_of) || !Ash.Resource.Info.resource?(instance_of) ->
        Ash.load(record, load, opts)

      # No primary read action, can't reasonably `load`
      is_nil(Ash.Resource.Info.primary_action(instance_of, :read)) ->
        {:ok, record}

      !opts[:authorize?] ->
        if load in [nil, [], %{}] do
          {:ok, record}
        else
          Ash.load(record, load, opts)
        end

      true ->
        # Two things to do here:
        #
        # 1. Scrub populated fields that aren't part of the explicit
        #    load via field policies (in memory — `apply_field_level_auth`
        #    doesn't re-fetch from the data layer).
        # 2. Run the explicit load through `Ash.load`, which goes through
        #    the read pipeline and applies field policies to loaded fields
        #    via the normal `restrict_field_access` step.
        with {:ok, scrubbed} <- scrub_populated(record, instance_of, load, opts) do
          if load in [nil, [], %{}] do
            {:ok, scrubbed}
          else
            Ash.load(scrubbed, load, opts)
          end
        end
    end
  end

  defp scrub_populated(record, resource, load, opts) do
    case populated_not_in_load(record, resource, load) do
      [] ->
        {:ok, record}

      to_scrub ->
        Ash.Authorizer.apply_field_level_auth(
          resource,
          record,
          Keyword.put(opts, :for_fields, to_scrub)
        )
    end
  end

  defp populated_not_in_load(record_or_records, resource, load) do
    sample =
      case record_or_records do
        [first | _] -> first
        other -> other
      end

    if is_struct(sample, resource) do
      load_names = load_top_level_names(load)

      resource
      |> Ash.Resource.Info.fields([:attributes, :calculations, :aggregates])
      |> Enum.filter(& &1.public?)
      |> Enum.map(& &1.name)
      |> Enum.filter(&(Ash.Resource.loaded?(sample, &1) and &1 not in load_names))
    else
      []
    end
  end

  defp load_top_level_names(load) when load in [nil, [], %{}], do: []

  defp load_top_level_names(load) when is_list(load) do
    Enum.flat_map(load, fn
      atom when is_atom(atom) -> [atom]
      {atom, _nested} when is_atom(atom) -> [atom]
      _ -> []
    end)
  end

  defp load_top_level_names(_), do: []

  @impl Ash.Type
  def merge_load(left, right, constraints, context) do
    instance_of = constraints[:instance_of]

    if instance_of do
      # instance_of_query = Ash.Query.new(instance_of)
      left = Ash.Query.load(instance_of, left)
      right = Ash.Query.load(instance_of, right)

      if left.valid? do
        {:ok, Ash.Query.merge_query_load(left, right, context)}
      else
        {:error, Ash.Error.to_ash_error(left.errors)}
      end
    else
      {:error, "Structs must have an `instance_of` constraint to be loaded through"}
    end
  end

  @impl Ash.Type
  def get_rewrites(merged_load, calculation, path, constraints) do
    instance_of = constraints[:instance_of]

    if instance_of && Ash.Resource.Info.resource?(instance_of) do
      merged_load = Ash.Query.load(instance_of, merged_load)
      Ash.Actions.Read.Calculations.get_all_rewrites(merged_load, calculation, path)
    else
      []
    end
  end

  @impl Ash.Type
  def rewrite(value, rewrites, _constraints) do
    Ash.Actions.Read.Calculations.rewrite(rewrites, value)
  end

  @impl Ash.Type
  def can_load?(constraints) do
    instance_of = constraints[:instance_of]

    instance_of && Ash.Resource.Info.resource?(instance_of)
  end

  defp handle_fields(value, constraints) do
    if fields = fields(constraints) do
      check_fields(value, fields)
    else
      {:ok, value}
    end
  end

  defp handle_instance_of(nil, _), do: {:ok, nil}

  defp handle_instance_of(value, constraints) do
    case Keyword.fetch(constraints, :instance_of) do
      {:ok, struct} ->
        cond do
          is_struct(value, struct) ->
            {:ok, value}

          is_struct(value) ->
            {:error, "is invalid"}

          fields(constraints) ->
            {:ok, struct(struct, value)}

          Ash.Resource.Info.resource?(struct) ->
            struct
            |> Ash.Resource.Info.public_attributes()
            |> Enum.reduce_while({:ok, struct.__struct__()}, fn attribute, {:ok, record} ->
              case fetch_field(value, attribute.name) do
                {:ok, value} ->
                  with {:ok, casted} <-
                         Ash.Type.cast_input(
                           attribute.type,
                           value,
                           attribute.constraints
                         ),
                       {:ok, casted} <-
                         Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
                    if is_nil(casted) and attribute.allow_nil? == false do
                      {:halt,
                       {:error, field: attribute.name, message: error_message("is required")}}
                    else
                      {:cont, {:ok, Map.put(record, attribute.name, casted)}}
                    end
                  else
                    :error ->
                      {:halt, {:error, "is invalid"}}

                    {:error, error} ->
                      {:halt, {:error, error}}
                  end

                :error ->
                  if attribute.allow_nil? == false do
                    {:halt,
                     {:error, field: attribute.name, message: error_message("is required")}}
                  else
                    {:cont, {:ok, record}}
                  end
              end
            end)

          true ->
            {:error, "is invalid"}
        end

      :error ->
        if is_struct(value) do
          {:ok, value}
        else
          {:error, "is invalid"}
        end
    end
  end

  defp check_fields(value, fields) do
    {errors, result} =
      Enum.reduce(fields, {[], %{}}, fn {field, field_constraints}, {errors_acc, result_acc} ->
        case fetch_field(value, field) do
          {:ok, field_value} ->
            case check_field(result_acc, field, field_value, field_constraints) do
              {:ok, updated_result} ->
                {errors_acc, updated_result}

              {:error, field_errors} ->
                {errors_acc ++ field_errors, result_acc}
            end

          :error ->
            if field_constraints[:allow_nil?] == false do
              field_error = [message: error_message("field must be present"), field: field]
              {errors_acc ++ [field_error], result_acc}
            else
              {errors_acc, result_acc}
            end
        end
      end)

    case errors do
      [] -> {:ok, result}
      _ -> {:error, errors}
    end
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
              {:error,
               [[message: error_message("value must not be nil"), field: field, value: nil]]}
            else
              {:ok, Map.put(result, field, nil)}
            end

          {:ok, field_value} ->
            {:ok, Map.put(result, field, field_value)}

          {:error, errors} ->
            formatted_errors =
              errors
              |> List.wrap()
              |> Enum.map(
                &Ash.Type.CompositeTypeHelpers.format_comprehensive_error_as_keyword(&1, field)
              )
              |> Ash.Type.CompositeTypeHelpers.filter_non_informative_errors()

            {:error, formatted_errors}
        end

      {:error, error} when is_binary(error) ->
        {:error, [[message: error, field: field, value: field_value]]}

      {:error, error} when is_list(error) ->
        if Keyword.keyword?(error) do
          {:error, [Keyword.update(error, :path, [field], &[field | &1])]}
        else
          {:error,
           Enum.map(error, fn error ->
             if Keyword.keyword?(error) do
               Keyword.update(error, :path, [field], &[field | &1])
             else
               error
             end
           end)}
        end

      {:error, error} ->
        {:error, error}

      :error ->
        {:error, [[message: error_message("invalid value"), field: field, value: field_value]]}
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
