# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions do
  @moduledoc "Types for Ash actions"
  alias Ash.Resource.Actions.{Action, Create, Destroy, Read, Update}

  @typedoc """
  The struct describing an action defined on a resource.

  Use `Ash.Resource.Info.action/3` to look up an action by name or
  `Ash.Resource.Info.actions/1` to list a resource's actions. All action structs
  have a `:name` and a `:type`. The remaining fields depend on the action type.

  For example, given a `:publish` update action on `MyApp.Post`:

      %Ash.Resource.Actions.Update{name: :publish, type: :update} =
        Ash.Resource.Info.action(MyApp.Post, :publish)

  See [Actions](/documentation/topics/actions/actions.md) for defining and running actions.
  """
  @type action :: Action.t() | Create.t() | Read.t() | Update.t() | Destroy.t()

  @typedoc """
  The kind of operation an action performs.

  The action struct's `:type` is one of `:read`, `:create`, `:update`,
  `:destroy`, or `:action`. The `:action` type represents a
  [generic action](/documentation/topics/actions/generic-actions.md).
  Multiple named actions can share the same type, such as `:publish`
  and `:archive` update actions.

  Use `Ash.Policy.Check.Builtins.action_type/1` to match actions by kind
  in an authorization policy.
  """
  @type action_type :: :action | :read | :create | :update | :destroy
end
