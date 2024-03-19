defmodule Ash.Reactor.ChangeStep do
  @moduledoc """
  The Reactor step which is used to execute change steps.
  """

  use Reactor.Step
  alias Ash.Changeset

  @doc false
  @impl true
  def run(arguments, context, options) do
    with {:ok, changeset} <- initial_changeset(arguments.initial, arguments.arguments),
         {:ok, changeset} <- maybe_must_be_valid(changeset, options[:only_when_valid?], :bypass),
         {:ok, changeset} <- apply_where_clauses(changeset, options[:where], context),
         {:ok, changeset} <- apply_change(changeset, options[:change], context) do
      fail_if_invalid? = Keyword.get(options, :fail_if_invalid?, false)
      maybe_must_be_valid(changeset, fail_if_invalid?, :error)
    else
      {:bypass, changeset} -> {:ok, changeset}
      {:error, reason} -> {:error, reason}
    end
  end

  defp initial_changeset(module, arguments) when is_atom(module),
    do: initial_changeset(Changeset.new(module), arguments)

  defp initial_changeset(changeset, arguments) when is_struct(changeset, Changeset),
    do: {:ok, Changeset.force_set_arguments(changeset, arguments)}

  defp maybe_must_be_valid(changeset, true, _) when changeset.valid?, do: {:ok, changeset}
  defp maybe_must_be_valid(changeset, true, tag), do: {tag, changeset}
  defp maybe_must_be_valid(changeset, _, _), do: {:ok, changeset}

  defp apply_where_clauses(changeset, nil, context),
    do: apply_where_clauses(changeset, [], context)

  defp apply_where_clauses(changeset, clauses, context) when is_list(clauses) do
    Enum.reduce_while(clauses, {:ok, changeset}, fn clause, {:ok, changeset} ->
      case apply_validation(changeset, clause, context) do
        :ok -> {:cont, {:ok, changeset}}
        {:error, _} -> {:halt, {:bypass, changeset}}
      end
    end)
  end

  defp apply_validation(changeset, {module, opts}, context) do
    opts = Ash.Expr.fill_template(opts, context[:actor], changeset.arguments, changeset.context)

    module.validate(changeset, opts, context)
  rescue
    error -> {:error, error}
  end

  defp apply_validation(changeset, module, context),
    do: apply_validation(changeset, {module, []}, context)

  defp apply_change(changeset, {module, opts}, context) do
    opts =
      Ash.Expr.fill_template(
        opts,
        context[:actor],
        changeset.arguments,
        changeset.context,
        changeset
      )

    with {:ok, opts} <- module.init(opts) do
      {:ok, module.change(changeset, opts, context)}
    end
  end

  defp apply_change(changeset, module, context) when is_atom(module),
    do: apply_change(changeset, {module, []}, context)
end
