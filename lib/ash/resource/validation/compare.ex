# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.Compare do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @opt_schema [
    greater_than: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should be greater than."
    ],
    greater_than_or_equal_to: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should be greater than or equal to"
    ],
    less_than: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should be less than"
    ],
    less_than_or_equal_to: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should be less than or equal to"
    ],
    is_equal: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should be equal to"
    ],
    is_not_equal: [
      type: {:or, [:any, {:tagged_tuple, :value, :any}, {:tagged_tuple, :ref, :atom}]},
      required: false,
      doc: "The value that the attribute should not be equal to"
    ],
    is_nil: [
      type: :boolean,
      required: false,
      doc: "Whether the attribute should be nil (true) or not nil (false)"
    ],
    attribute: [
      type: :atom,
      hide: true
    ]
  ]

  def opt_schema, do: @opt_schema

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

  @impl true
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def supports(_), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, opts, _context) do
    value =
      if Enum.any?(subject.action.arguments, &(&1.name == opts[:attribute])) do
        Ash.Subject.fetch_argument(subject, opts[:attribute])
      else
        case subject do
          %Ash.Changeset{} -> {:ok, Ash.Changeset.get_attribute(subject, opts[:attribute])}
          _ -> :error
        end
      end

    case value do
      {:ok, nil} ->
        if opts[:is_nil] == false do
          invalid_attribute_error(opts, nil)
        else
          :ok
        end

      {:ok, value} ->
        validate_value(subject, opts, value)

      _ ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    if Enum.any?(changeset.action.arguments, &(&1.name == opts[:attribute])) do
      # (!Keyword.has_key?(changeset.atomics, opts[:attribute]) &&
      #    Map.has_key?(changeset.attributes, opts[:attribute])) do
      validate(changeset, opts, context)
    else
      opts
      |> Keyword.take([
        :greater_than,
        :less_than,
        :greater_than_or_equal_to,
        :less_than_or_equal_to,
        :is_equal,
        :is_not_equal,
        :is_nil
      ])
      |> Enum.map(fn
        {:greater_than, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) <= ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must be greater than %{greater_than}"),
               vars: %{field: ^opts[:attribute], greater_than: ^atomic_value(value)}
             })
           )}

        {:less_than, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) >= ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must be less than %{less_than}"),
               vars: %{field: ^opts[:attribute], less_than: ^atomic_value(value)}
             })
           )}

        {:greater_than_or_equal_to, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) < ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message:
                 ^(context.message ||
                     "must be greater than or equal to %{greater_than_or_equal_to}"),
               vars: %{field: ^opts[:attribute], greater_than_or_equal_to: ^atomic_value(value)}
             })
           )}

        {:less_than_or_equal_to, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) > ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message:
                 ^(context.message || "must be less than or equal to %{less_than_or_equal_to}"),
               vars: %{field: ^opts[:attribute], less_than_or_equal_to: ^atomic_value(value)}
             })
           )}

        {:is_equal, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) != ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must be equal to %{is_equal}"),
               vars: %{field: ^opts[:attribute], is_equal: ^atomic_value(value)}
             })
           )}

        {:is_not_equal, value} ->
          {:atomic, [opts[:attribute]],
           expr(^atomic_ref(opts[:attribute]) == ^atomic_value(value)),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must not be equal to %{is_not_equal}"),
               vars: %{field: ^opts[:attribute], is_not_equal: ^atomic_value(value)}
             })
           )}

        {:is_nil, true} ->
          {:atomic, [opts[:attribute]], expr(not is_nil(^atomic_ref(opts[:attribute]))),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must be nil"),
               vars: %{field: ^opts[:attribute]}
             })
           )}

        {:is_nil, false} ->
          {:atomic, [opts[:attribute]], expr(is_nil(^atomic_ref(opts[:attribute]))),
           expr(
             error(^InvalidAttribute, %{
               field: ^opts[:attribute],
               value: ^atomic_ref(opts[:attribute]),
               message: ^(context.message || "must not be nil"),
               vars: %{field: ^opts[:attribute]}
             })
           )}
      end)
    end
  end

  @impl true
  def describe(opts) do
    [
      vars:
        [
          greater_than: opts[:greater_than],
          less_than: opts[:less_than],
          greater_than_or_equal_to: opts[:greater_than_or_equal_to],
          less_than_or_equal_to: opts[:less_than_or_equal_to],
          is_equal: opts[:is_equal],
          is_not_equal: opts[:is_not_equal],
          is_nil: opts[:is_nil]
        ]
        |> Enum.map(fn {k, v} ->
          case v do
            fun when is_function(fun, 0) -> {k, fun.()}
            v when is_struct(v) -> {k, v}
            v when is_list(v) -> {k, format_value(v)}
            v when is_map(v) -> {k, format_value(v)}
            v when is_tuple(v) -> {k, format_value(v)}
            v -> {k, v}
          end
        end),
      message: opts[:message] || message(opts)
    ]
  end

  defp validate_value(changeset, opts, value) do
    cond do
      opts[:is_nil] == true && !is_nil(value) ->
        invalid_attribute_error(opts, value)

      opts[:is_nil] == false && is_nil(value) ->
        invalid_attribute_error(opts, value)

      is_nil(value) ->
        :ok

      true ->
        validate_comparisons(changeset, opts, value)
    end
  end

  defp validate_comparisons(subject, opts, value) do
    opts
    |> Keyword.take([
      :greater_than,
      :less_than,
      :greater_than_or_equal_to,
      :less_than_or_equal_to,
      :is_equal,
      :is_not_equal
    ])
    |> Enum.find_value(fn
      {:greater_than, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.greater_than?/2, opts)

      {:greater_than_or_equal_to, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.greater_or_equal?/2, opts)

      {:less_than, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.less_than?/2, opts)

      {:less_than_or_equal_to, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.less_or_equal?/2, opts)

      {:is_equal, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.equal?/2, opts)

      {:is_not_equal, attribute} ->
        attribute_val = normalize_value(attribute_value(subject, attribute))
        compare_values(value, attribute_val, &Comp.not_equal?/2, opts)
    end) || :ok
  end

  defp normalize_value(value) do
    case value do
      {:value, val} -> val
      _ -> value
    end
  end

  defp compare_values(value, compare_to, comparison_fn, opts) do
    if !comparison_fn.(value, compare_to) do
      invalid_attribute_error(opts, value)
    end
  end

  defp format_value(value) when is_list(value) do
    items = Enum.map_join(value, ", ", &format_value/1)
    "[#{items}]"
  end

  defp format_value(value) when is_map(value) do
    items =
      value
      |> Enum.sort_by(fn {k, _v} -> to_string(k) end)
      |> Enum.map_join(", ", fn {k, v} -> "#{k}: #{format_value(v)}" end)

    "%{#{items}}"
  end

  defp format_value(value) when is_tuple(value) do
    case value do
      {:value, val} ->
        val

      _ ->
        items = value |> Tuple.to_list() |> Enum.map_join(", ", &format_value/1)
        "{#{items}}"
    end
  end

  defp format_value(value), do: value

  defp attribute_value(_changeset, attribute) when is_function(attribute, 0) do
    attribute.()
  end

  defp attribute_value(subject, attribute) when is_atom(attribute) do
    Ash.Subject.get_argument_or_attribute(subject, attribute)
  end

  defp attribute_value(_subject, attribute), do: attribute

  defp atomic_value(attribute) when is_function(attribute, 0) do
    attribute.()
  end

  defp atomic_value(attribute) when is_atom(attribute) do
    atomic_ref(attribute)
  end

  defp atomic_value(attribute), do: attribute

  defp invalid_attribute_error(opts, attribute_value) do
    value =
      if is_function(attribute_value) do
        attribute_value.()
      else
        attribute_value
      end

    {:error,
     [
       field: opts[:attribute],
       value: value
     ]
     |> with_description(opts)
     |> InvalidAttribute.exception()}
  end

  defp message(opts) do
    opts
    |> Keyword.take([
      :greater_than,
      :less_than,
      :greater_than_or_equal_to,
      :less_than_or_equal_to,
      :is_equal,
      :is_not_equal,
      :is_nil
    ])
    |> Enum.map_join(" and ", fn {key, value} ->
      case key do
        :greater_than ->
          "must be greater than %{greater_than}"

        :less_than ->
          "must be less than %{less_than}"

        :greater_than_or_equal_to ->
          "must be greater than or equal to %{greater_than_or_equal_to}"

        :less_than_or_equal_to ->
          "must be less than or equal to %{less_than_or_equal_to}"

        :is_equal ->
          "must be equal to %{is_equal}"

        :is_not_equal ->
          "must not be equal to %{is_not_equal}"

        :is_nil when value == true ->
          "must be nil"

        :is_nil when value == false ->
          "must not be nil"
      end
    end)
  end
end
