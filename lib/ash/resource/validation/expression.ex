# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.Expression do
  @moduledoc false

  use Ash.Resource.Validation

  @impl true
  def temporal_safe?(_opts), do: true

  @impl true
  def validate(changeset, opts, context) do
    {:ok, record} =
      Ash.Changeset.apply_attributes(changeset, force?: true)

    condition_expr =
      Ash.Expr.fill_template(
        opts[:condition],
        source_context: changeset.context,
        actor: context.actor,
        tenant: context.tenant,
        args: changeset.arguments,
        context: changeset.context,
        changeset: changeset
      )

    error_expr =
      Ash.Expr.fill_template(
        opts[:error_expr],
        source_context: changeset.context,
        actor: context.actor,
        tenant: context.tenant,
        args: changeset.arguments,
        context: changeset.context,
        changeset: changeset
      )

    eval_opts = [
      resource: changeset.resource,
      record: record,
      domain: changeset.domain,
      actor: context.actor,
      tenant: context.tenant
    ]

    case Ash.Expr.eval(condition_expr, eval_opts) do
      {:ok, value} when value in [false, nil] ->
        :ok

      {:ok, _value} ->
        case Ash.Expr.eval(error_expr, eval_opts) do
          {:error, error} -> {:error, error}
          {:ok, _value} -> :ok
        end

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def atomic(_changeset, opts, _context) do
    {:atomic, :*, opts[:condition], opts[:error_expr]}
  end
end
