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
      type: {:or, [:integer, :atom, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be greater than."
    ],
    greater_than_or_equal_to: [
      type: {:or, [:integer, :atom, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be greater than or equal to"
    ],
    less_than: [
      type: {:or, [:integer, :atom, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be less than"
    ],
    less_than_or_equal_to: [
      type: {:or, [:integer, :atom, {:fun, 0}]},
      required: false,
      doc: "The value that the attribute should be less than or equal to"
    ]
  ]

  def opt_schema, do: @opt_schema

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
                  else:
                    invalid_attribute_error(
                      Keyword.put(opts, :value, attribute),
                      "must be greater than %{value}"
                    )

              {:greater_than_or_equal_to, attribute} ->
                if Comp.greater_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else:
                    invalid_attribute_error(
                      Keyword.put(opts, :value, attribute),
                      "must be greater than or equal to %{value}"
                    )

              {:less_than, attribute} ->
                if Comp.less_than?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else:
                    invalid_attribute_error(
                      Keyword.put(opts, :value, attribute),
                      "must be less than %{value}"
                    )

              {:less_than_or_equal_to, attribute} ->
                if Comp.less_or_equal?(value, attribute_value(changeset, attribute)),
                  do: :ok,
                  else:
                    invalid_attribute_error(
                      Keyword.put(opts, :value, attribute),
                      "must be less than or equal to %{value}"
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

  defp attribute_value(_changeset, attribute) when is_function(attribute, 0) do
    attribute.()
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
         value:
           case opts[:value] do
             fun when is_function(fun, 0) -> fun.()
             v -> v
           end,
         greater_than: opts[:greater_than],
         less_than: opts[:less_than],
         greater_than_or_equal_to: opts[:greater_than_or_equal_to],
         less_than_or_equal_to: opts[:less_than_or_equal_to]
       ]
     )}
  end
end
