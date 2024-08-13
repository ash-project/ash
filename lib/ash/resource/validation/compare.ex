defmodule Ash.Resource.Validation.Compare do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @opt_schema [
    greater_than: [
      type: {:or, [:integer, :atom, :float, {:struct, Decimal}, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be greater than."
    ],
    greater_than_or_equal_to: [
      type: {:or, [:integer, :atom, :float, {:struct, Decimal}, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be greater than or equal to"
    ],
    less_than: [
      type: {:or, [:integer, :atom, :float, {:struct, Decimal}, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be less than"
    ],
    less_than_or_equal_to: [
      type: {:or, [:integer, :atom, :float, {:struct, Decimal}, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be less than or equal to"
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
  def validate(changeset, opts, _context) do
    value =
      if Enum.any?(changeset.action.arguments, &(&1.name == opts[:attribute])) do
        Ash.Changeset.fetch_argument(changeset, opts[:attribute])
      else
        {:ok, Ash.Changeset.get_attribute(changeset, opts[:attribute])}
      end

    case value do
      {:ok, value} when not is_nil(value) ->
        opts
        |> Keyword.take([
          :greater_than,
          :less_than,
          :greater_than_or_equal_to,
          :less_than_or_equal_to
        ])
        |> Enum.find_value(fn
          {:greater_than, attribute} ->
            if !Comp.greater_than?(value, attribute_value(changeset, attribute)),
              do: invalid_attribute_error(opts, value)

          {:greater_than_or_equal_to, attribute} ->
            if !Comp.greater_or_equal?(value, attribute_value(changeset, attribute)),
              do: invalid_attribute_error(opts, value)

          {:less_than, attribute} ->
            if !Comp.less_than?(value, attribute_value(changeset, attribute)),
              do: invalid_attribute_error(opts, value)

          {:less_than_or_equal_to, attribute} ->
            if !Comp.less_or_equal?(value, attribute_value(changeset, attribute)),
              do: invalid_attribute_error(opts, value)
        end) || :ok

      _ ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    if Enum.any?(changeset.action.arguments, &(&1.name == opts[:attribute])) do
      validate(changeset, opts, context)
    else
      opts
      |> Keyword.take([
        :greater_than,
        :less_than,
        :greater_than_or_equal_to,
        :less_than_or_equal_to
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
      end)
    end
  end

  @impl true
  def describe(opts) do
    [
      vars: [
        value:
          case opts[:value] do
            fun when is_function(fun, 0) -> fun.()
            v -> v
          end,
        greater_than: opts[:greater_than],
        less_than: opts[:less_than],
        greater_than_or_equal_to: opts[:greater_than_or_equal_to],
        less_than_or_equal_to: opts[:less_than_or_equal_to]
      ],
      message: opts[:message] || message(opts)
    ]
  end

  defp attribute_value(_changeset, attribute) when is_function(attribute, 0) do
    attribute.()
  end

  defp attribute_value(changeset, attribute) when is_atom(attribute),
    do: Ash.Changeset.get_argument_or_attribute(changeset, attribute)

  defp attribute_value(_, attribute), do: attribute

  defp atomic_value(attribute) when is_function(attribute, 0) do
    attribute.()
  end

  defp atomic_value(attribute) when is_atom(attribute) do
    atomic_ref(attribute)
  end

  defp atomic_value(attribute), do: attribute

  defp invalid_attribute_error(opts, attribute_value) do
    {:error,
     [
       field: opts[:attribute],
       value: attribute_value
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
      :less_than_or_equal_to
    ])
    |> Enum.map_join(" and ", fn {key, _value} ->
      case key do
        :greater_than ->
          "must be greater than %{greater_than}"

        :less_than ->
          "must be less than %{less_than}"

        :greater_than_or_equal_to ->
          "must be greater than or equal to %{greater_than_or_equal_to}"

        :less_than_or_equal_to ->
          "must be less than or equal to %{less_than_or_equal_to}"
      end
    end)
  end
end
