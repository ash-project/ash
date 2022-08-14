defmodule Ash.Resource.Validation.Compare do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    greater_than: [
      type: {:or, [:integer, :atom]},
      required: false,
      doc: "The value that the attribute should be greater than"
    ],
    greater_than_or_equal_to: [
      type: {:or, [:integer, :atom]},
      required: false,
      doc: "The value that the attribute should be greater than or equal to"
    ],
    less_than: [
      type: {:or, [:integer, :atom]},
      required: false,
      doc: "The value that the attribute should be less than"
    ],
    less_than_or_equal_to: [
      type: {:or, [:integer, :atom]},
      required: false,
      doc: "The value that the attribute should be less than or equal to"
    ]
  ]

  @impl true
  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
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
                  else: invalid_attribute_error(opts, "must be greater than #{attribute}")

              {:greater_than_or_equal_to, attribute} ->
                if Comp.greater_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else:
                    invalid_attribute_error(
                      opts,
                      "must be greater than or equal to #{attribute}"
                    )

              {:less_than, attribute} ->
                if Comp.less_than?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else: invalid_attribute_error(opts, "must be less than #{attribute}")

              {:less_than_or_equal_to, attribute} ->
                if Comp.less_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else:
                    invalid_attribute_error(
                      opts,
                      "must be less than or equal to #{attribute}"
                    )

              true ->
                :ok
            end
          end
        )

      _ ->
        :ok
    end
  end

  defp attribute_value(changeset, attribute) when is_atom(attribute),
    do: Ash.Changeset.get_argument_or_attribute(changeset, attribute)

  defp attribute_value(_, attribute), do: attribute

  defp invalid_attribute_error(opts, message) do
    {:error,
     InvalidAttribute.exception(
       field: opts[:attribute],
       message: opts[:message] || message,
       vars: [
         greater_than: opts[:greater_than],
         less_than: opts[:less_than],
         greater_than_or_equal_to: opts[:greater_than_or_equal_to],
         less_than_or_equal_to: opts[:less_than_or_equal_to]
       ]
     )}
  end
end
