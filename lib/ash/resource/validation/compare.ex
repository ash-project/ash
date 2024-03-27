defmodule Ash.Resource.Validation.Compare do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  import Ash.Expr

  @impl true
  def init(opts) do
    case Spark.Options.validate(
           opts,
           Keyword.put(Ash.Resource.Validation.Builtins.compare_opts(), :attribute,
             type: :atom,
             required: true
           )
         ) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    case Ash.Changeset.fetch_argument_or_change(changeset, opts[:attribute]) do
      {:ok, value} ->
        Enum.reduce(
          Keyword.take(opts, [
            :greater_than,
            :less_than,
            :greater_than_or_equal_to,
            :less_than_or_equal_to
          ]),
          :ok,
          fn validation, _ ->
            case validation do
              {:greater_than, attribute} ->
                if Comp.greater_than?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else: invalid_attribute_error(opts, value)

              {:greater_than_or_equal_to, attribute} ->
                if Comp.greater_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else: invalid_attribute_error(opts, value)

              {:less_than, attribute} ->
                if Comp.less_than?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else: invalid_attribute_error(opts, value)

              {:less_than_or_equal_to, attribute} ->
                if Comp.less_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else: invalid_attribute_error(opts, value)

              true ->
                :ok
            end
          end
        )

      _ ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    case Ash.Changeset.fetch_argument(changeset, opts[:attribute]) do
      :error ->
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

      {:ok, _} ->
        validate(changeset, opts, context)
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
