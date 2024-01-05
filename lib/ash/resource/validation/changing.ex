defmodule Ash.Resource.Validation.Changing do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute
  require Ash.Expr

  @opt_schema [
    field: [
      type: :atom,
      required: true,
      doc: "The attribute to check"
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
    case Ash.Resource.Info.relationship(changeset.resource, opts[:field]) do
      nil ->
        if Ash.Changeset.changing_attribute?(changeset, opts[:field]) do
          :ok
        else
          {:error, exception(opts)}
        end

      %{type: :belongs_to, source_attribute: source_attribute} = relationship ->
        if Ash.Changeset.changing_attribute?(changeset, source_attribute) ||
             Ash.Changeset.changing_relationship?(changeset, relationship.name) do
          :ok
        else
          {:error, exception(opts)}
        end

      relationship ->
        if Ash.Changeset.changing_relationship?(changeset, relationship.name) do
          :ok
        else
          {:error, exception(opts)}
        end
    end
  end

  @impl true
  def atomic(changeset, opts) do
    new_value = Ash.Changeset.atomic_ref(changeset, opts[:field])
    old_value = Ash.Expr.expr(ref(^opts[:field]))

    {:atomic, [opts[:field]], Ash.Expr.expr(^new_value != ^old_value),
     Ash.Expr.expr(
       error(^InvalidAttribute, %{
         field: ^opts[:field],
         value: ^new_value,
         message: "must be changing",
         vars: %{field: ^opts[:field]}
       })
     )}
  end

  @impl true
  def describe(_opts) do
    [
      message: "must be changing",
      vars: []
    ]
  end

  defp exception(opts) do
    [field: opts[:field]]
    |> with_description(opts)
    |> InvalidAttribute.exception()
  end
end
