defmodule Ash.Resource.Validation.Changing do
  @moduledoc false

  use Ash.Resource.Validation

  import Ash.Changeset

  alias Ash.Error.Changes.InvalidAttribute

  defmodule Opts do
    @moduledoc false

    use Spark.Options.Validator,
      schema: [
        field: [
          type: :atom,
          required: true,
          doc: "The attribute or relationship to check for changes."
        ]
      ]
  end

  @default_error_message "must be changing"

  @impl Ash.Resource.Validation
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl Ash.Resource.Validation
  def validate(changeset, opts, context) do
    if field_is_changing?(changeset, Keyword.fetch!(opts, :field)),
      do: :ok,
      else: {:error, exception(opts, context)}
  end

  defp field_is_changing?(changeset, field) do
    case Ash.Resource.Info.relationship(changeset.resource, field) do
      nil ->
        changing_attribute?(changeset, field)

      %{type: :belongs_to} = relationship ->
        changing_attribute?(changeset, relationship.source_attribute) ||
          changing_relationship?(changeset, relationship.name)

      relationship ->
        changing_relationship?(changeset, relationship.name)
    end
  end

  @impl Ash.Resource.Validation
  def atomic(changeset, opts, context) do
    case atomic_field(changeset, Keyword.fetch!(opts, :field)) do
      {:changing, field} ->
        {:atomic, [field], expr(^atomic_ref(field) == ^ref(field)),
         expr(
           error(^InvalidAttribute, %{
             field: ^field,
             value: ^atomic_ref(field),
             message: ^(context.message || @default_error_message),
             vars: %{field: ^field}
           })
         )}

      {:not_changing, _field} ->
        {:error, exception(opts, context)}

      {:not_atomic, field} ->
        {:not_atomic, "can't atomically check if #{field} relationship is changing"}
    end
  end

  defp atomic_field(changeset, field) do
    changeset.resource
    |> Ash.Resource.Info.relationship(field)
    |> case do
      nil -> {:atomic, field}
      %{type: :belongs_to} = relationship -> {:atomic, relationship.source_attribute}
      _relationship -> {:not_atomic, field}
    end
    |> atomic_result(changeset)
  end

  defp atomic_result({:atomic, field}, changeset) do
    msg =
      if changing_attribute?(changeset, field),
        do: :changing,
        else: :not_changing

    {msg, field}
  end

  defp atomic_result(not_atomic, _changeset), do: not_atomic

  @impl Ash.Resource.Validation
  def describe(_opts), do: [message: @default_error_message, vars: []]

  defp exception(opts, context) do
    [field: opts[:field]]
    |> with_description(opts)
    |> maybe_use_context_message(context.message)
    |> InvalidAttribute.exception()
  end

  defp maybe_use_context_message(opts, nil), do: opts

  defp maybe_use_context_message(opts, message) do
    Keyword.put(opts, :message, message)
  end
end
