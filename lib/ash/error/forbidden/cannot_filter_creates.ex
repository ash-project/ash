# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Forbidden.CannotFilterCreates do
  @moduledoc "Used when a create action would be filtered"

  use Splode.Error,
    fields: [:filter, :resource, :action, :reason],
    class: :forbidden

  def message(error) do
    filter =
      case error.filter do
        %Ash.Filter{expression: expression} -> expression
        nil -> nil
        other -> other
      end

    where =
      case {error.resource, error.action} do
        {nil, nil} -> ""
        {resource, nil} -> "\n\nResource: #{inspect(resource)}"
        {resource, action} -> "\n\nResource: #{inspect(resource)}, action: #{inspect(action)}"
      end

    transactional? =
      error.resource && Ash.DataLayer.data_layer_can?(error.resource, :transact)

    transactional_note =
      cond do
        error.reason == :non_transactional_hooks ->
          """
          \nThe action has `before_transaction` and/or `around_transaction` hooks. Those run
          *outside* the data-layer transaction, so any side effects they perform cannot be
          rolled back when the post-insert filter check rejects the row. Ash refuses to
          install post-action authorization in this configuration by default.

          To opt in, set `allow_post_action_authorization? true` on the create action,
          acknowledging that the non-transactional hooks may fire for create requests that
          ultimately get rejected:

              actions do
                create :create do
                  allow_post_action_authorization? true
                end
              end

          Alternatively, remove the `before_transaction` / `around_transaction` hooks, or
          rewrite the policy so it does not need a filter on this create action.\n
          """

        error.resource == nil ->
          ""

        not transactional? ->
          """
          \nThe resource's data layer (#{inspect(Ash.DataLayer.data_layer(error.resource))}) does not
          support transactions, so Ash cannot safely roll back the row if the filter rejects it
          post-insert. Filter-mode policy checks on create actions are only supported on
          transactional data layers.\n
          """

        error.action && error.action != :__missing__ ->
          action = Ash.Resource.Info.action(error.resource, error.action)

          if action && action.transaction? == false do
            """
            \nThe action `#{inspect(error.action)}` has `transaction? false`, so Ash cannot
            safely roll back the row if the filter rejects it post-insert. Either remove
            `transaction? false` from the action or rewrite the policy to avoid filter-mode
            checks on this action.\n
            """
          else
            ""
          end

        true ->
          ""
      end

    filter_line =
      if filter, do: "\n\nFilter: #{inspect(filter)}", else: ""

    """
    Cannot use a filter to authorize a create.#{where}#{filter_line}
    #{transactional_note}
    Ash normally defers filter-mode policy checks on create actions to a post-insert
    authorization step inside the action's transaction — the inserted record is loaded back
    and the filter is evaluated against it, and the transaction is rolled back if the row
    does not match. This error means that deferral could not be set up — typically because:

      * A non-policy authorizer returned a filter for a create changeset (only
        `Ash.Policy.Authorizer` is wired into the post-insert path today).
      * A `base_query` carrying a filter was attached to the create changeset.
      * The check was rendered with `filter_with: :error`, which forces synchronous evaluation.
      * The data layer does not support transactions, or the action sets `transaction? false`.

    If you want to keep authorization synchronous for create actions, narrow the policy
    `condition` so it does not apply to creates, and add a create-specific policy whose
    check is evaluable against the changeset itself (e.g. `changing_attributes/1` or an
    actor/context-only expression).

      Given a policy like:

          policy expr(special == true) do
            authorize_if expr(allows_special == true)
          end

      You would rewrite it to not include create actions like so:

          policy [expr(special == true), action_type([:read, :update, :destroy])] do
            authorize_if expr(allows_special == true)
          end

      At which point you could add a `create` specific policy:

          policy [changing_attributes(special: [to: true]), action_type(:create)] do
            authorize_if changing_attributes(special: [to: true])
          end
    """
  end
end
