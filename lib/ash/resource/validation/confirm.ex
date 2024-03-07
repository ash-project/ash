defmodule Ash.Resource.Validation.Confirm do
  @moduledoc false
  use Ash.Resource.Validation
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidAttribute
  require Ash.Expr

  @impl true
  def init(opts) do
    case opts[:field] do
      nil ->
        {:error, "Field is required"}

      field when is_atom(field) ->
        case opts[:confirmation] do
          nil ->
            {:error, "Confirmation is required"}

          confirmation when is_atom(confirmation) ->
            {:ok, [confirmation: confirmation, field: field]}

          confirmation ->
            {:error, "Expected an atom for confirmation, got: #{inspect(confirmation)}"}
        end

      field ->
        {:error, "Expected an atom for field, got: #{inspect(field)}"}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    confirmation_value =
      Changeset.get_argument(changeset, opts[:confirmation]) ||
        Changeset.get_attribute(changeset, opts[:confirmation])

    value =
      Changeset.get_argument(changeset, opts[:field]) ||
        Changeset.get_attribute(changeset, opts[:field])

    if Comp.equal?(confirmation_value, value) do
      :ok
    else
      {:error,
       [field: opts[:confirmation], value: confirmation_value]
       |> with_description(opts)
       |> InvalidAttribute.exception()}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    confirmation =
      case Changeset.fetch_argument_or_change(changeset, opts[:confirmation]) do
        {:ok, value} ->
          value

        :error ->
          Changeset.atomic_ref(changeset, opts[:confirmation])
      end

    value =
      case Changeset.fetch_argument_or_change(changeset, opts[:field]) do
        {:ok, value} ->
          value

        :error ->
          Changeset.atomic_ref(changeset, opts[:field])
      end

    {:atomic, [opts[:confirmation], opts[:field]], Ash.Expr.expr(^confirmation != ^value),
     Ash.Expr.expr(
       error(^InvalidAttribute, %{
         field: ^opts[:confirmation],
         value: ^value,
         message: ^(context.message || "confirmation did not match value")
       })
     )}
  end

  @impl true
  def describe(_opts) do
    [
      message: "confirmation did not match value",
      vars: []
    ]
  end
end
