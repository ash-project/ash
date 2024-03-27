defmodule Ash.Error.Forbidden.CannotFilterCreates do
  @moduledoc "Used when a create action would be filtered"
  use Ash.Error.Exception

  use Splode.Error, fields: [], class: :forbidden

  def message(_) do
    """
    Cannot use a filter to authorize a create.

    If you are using Ash.Policy.Authorizer:

      Many expressions, like those that reference relationships, require using custom checks for create actions.

      Expressions that only reference the actor or context, for example `expr(^actor(:is_admin) == true)` will work
      because those are evaluated without needing to reference data.

      For create actions, there is no data yet. In the future we may support referencing simple attributes and those
      references will be referring to the values of the data about to be created, but at this time we do not.

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
