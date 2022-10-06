defmodule Ash.Error.Forbidden.CannotFilterCreates do
  @moduledoc "Used when a create action would be filtered"
  use Ash.Error.Exception

  def_ash_error([], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "cannot_filter_creates"

    def message(_) do
      """
      Filter checks cannot be used with create actions.

      If you are using Ash.Policy.Authorizer:

        To solve for this, use other checks, or write a custom check.

        Many expressions, like those that reference relationships, require using custom checks for create actions.

        Expressions that only reference the actor or context, for example `expr(^actor(:is_admin) == true)` will work fine.

        Given a policy like:

        ```elixir
        policy expr(special == true) do
        authorize_if expr(allows_special == true)
        end
        ```

        You would rewrite it to not include create actions like so:

        ```elixir
        policy [expr(special == true), action_type([:read, :update, :destroy])] do
        authorize_if expr(allows_special == true)
        end
        ```

        At which point you could add a `create` specific policy:

        ```elixir
        policy [changing_attributes(special: [to: true]), action_type(:create)] do
        authorize_if changing_attributes(special: [to: true])
        end
        ```

        In these cases, you may also end up wanting to write a custom check.
      """
    end
  end
end
