defmodule Ash.Resource.Validation.Match do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    attribute: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
    ],
    match: [
      type: :any,
      required: true,
      doc: "The value that the attribute should match against"
    ],
    message: [
      type: :string,
      required: true,
      doc: "The message that will be placed on the field in the case of failure"
    ]
  ]

  @impl true
  def init(opts) do
    case Spark.Options.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    case Ash.Changeset.fetch_argument_or_change(changeset, opts[:attribute]) do
      {:ok, changing_to} when not is_nil(changing_to) ->
        case string_value(changing_to, opts) do
          {:ok, changing_to} ->
            if String.match?(changing_to, opts[:match]) do
              :ok
            else
              {:error, exception(opts)}
            end

          {:error, error} ->
            {:error, error}
        end

      _ ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    cond do
      argument?(changeset.action, opts[:attribute]) ->
        validate(changeset, opts, context)

      not Ash.Changeset.changing_attribute?(changeset, opts[:attribute]) ->
        {:not_atomic, "can't atomically match an attribute that is not changing"}

      atomic_expr_change?(changeset.atomics, opts[:attribute]) ->
        {:not_atomic, "can't match on an atomic expression"}

      true ->
        validate(changeset, opts, context)
    end
  end

  defp argument?(%{arguments: arguments}, attribute) do
    Enum.find(arguments, &(&1.name == attribute))
  end

  defp atomic_expr_change?(atomics, attribute) do
    case Keyword.get(atomics, attribute) do
      nil -> false
      value -> Ash.Expr.expr?(value)
    end
  end

  @impl true
  def describe(opts) do
    [
      message: opts[:message],
      vars: [field: opts[:attribute], match: opts[:match]]
    ]
  end

  defp string_value(value, opts) do
    {:ok, to_string(value)}
  rescue
    _ ->
      {:error, exception(opts)}
  end

  defp exception(opts) do
    [value: opts[:value], field: opts[:attribute]]
    |> with_description(opts)
    |> InvalidAttribute.exception()
  end
end
