# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Reactor.ChangeStep do
  @moduledoc """
  The Reactor step which is used to execute change steps.
  """

  use Reactor.Step
  alias Ash.Changeset

  @doc false
  @impl true
  def run(arguments, context, options) do
    changeset = initial_changeset(arguments.initial)
    step_arguments = Map.new(arguments.arguments || %{})
    original_arguments = changeset.arguments

    # Step arguments are scoped to this step only. They are made available to
    # the `where` validations and the change (both via `arg/1` templates and
    # `Ash.Changeset.get_argument/2`), but they are *not* action arguments, so
    # we bypass `set_argument/3` (which validates against the action) and
    # restore the original arguments once the change has been applied.
    changeset = %{changeset | arguments: Map.merge(original_arguments, step_arguments)}

    result =
      with {:ok, changeset} <- maybe_must_be_valid(changeset, options[:only_when_valid?], :bypass),
           {:ok, changeset} <- apply_where_clauses(changeset, options[:where], context),
           {:ok, changeset} <- apply_change(changeset, options[:change], context) do
        fail_if_invalid? = Keyword.get(options, :fail_if_invalid?, false)
        maybe_must_be_valid(changeset, fail_if_invalid?, :error)
      else
        {:bypass, changeset} -> {:ok, changeset}
        {:error, reason} -> {:error, reason}
      end

    case result do
      {:ok, changeset} ->
        {:ok, restore_arguments(changeset, step_arguments, original_arguments)}

      other ->
        other
    end
  end

  defp initial_changeset(module) when is_atom(module), do: Changeset.new(module)
  defp initial_changeset(changeset) when is_struct(changeset, Changeset), do: changeset

  defp restore_arguments(changeset, step_arguments, original_arguments) do
    arguments =
      Enum.reduce(step_arguments, changeset.arguments, fn {key, _}, arguments ->
        case Map.fetch(original_arguments, key) do
          {:ok, original} -> Map.put(arguments, key, original)
          :error -> Map.delete(arguments, key)
        end
      end)

    %{changeset | arguments: arguments}
  end

  defp maybe_must_be_valid(changeset, true, _) when changeset.valid?, do: {:ok, changeset}
  defp maybe_must_be_valid(changeset, true, tag), do: {tag, changeset}
  defp maybe_must_be_valid(changeset, _, _), do: {:ok, changeset}

  defp apply_where_clauses(changeset, nil, context),
    do: apply_where_clauses(changeset, [], context)

  defp apply_where_clauses(changeset, clauses, context) when is_list(clauses) do
    Enum.reduce_while(clauses, {:ok, changeset}, fn clause, {:ok, changeset} ->
      case apply_validation(changeset, clause, context) do
        :ok -> {:cont, {:ok, changeset}}
        {:raised, error} -> {:halt, {:error, error}}
        {:error, _} -> {:halt, {:bypass, changeset}}
      end
    end)
  end

  defp apply_validation(changeset, {module, opts}, context) do
    opts =
      Ash.Expr.fill_template(
        opts,
        actor: context[:actor],
        tenant: changeset.to_tenant,
        args: changeset.arguments,
        context: changeset.context,
        changeset: changeset
      )

    Ash.Resource.Validation.validate(module, changeset, opts, context)
  rescue
    error -> {:raised, error}
  end

  defp apply_validation(changeset, module, context),
    do: apply_validation(changeset, {module, []}, context)

  defp apply_change(changeset, {module, opts}, context) do
    opts =
      Ash.Expr.fill_template(
        opts,
        actor: context[:actor],
        tenant: changeset.to_tenant,
        args: changeset.arguments,
        context: changeset.context,
        changeset: changeset
      )

    with {:ok, opts} <- Ash.Resource.Change.init(module, opts) do
      {:ok, Ash.Resource.Change.change(module, changeset, opts, context)}
    end
  end

  defp apply_change(changeset, module, context) when is_atom(module),
    do: apply_change(changeset, {module, []}, context)
end
