defmodule Ash.Notifier.Notification do
  @moduledoc """
  Represents a notification that will be handled by a resource's notifiers

  Set the `for` key to a notifier or a list of notifiers to route the notification
  to them. This allows you to produce notifications inside of a `change` module
  and target specific notifiers with them.

  `metadata` is freeform data to be set however you want. `resource`, `action`, `data`,
  `changeset` and `actor` are all set by default based on the details of the action, so
  they can be omitted.

  When creating a notification, a resource is required to ensure that the notification isn't
  sent until the current transaction for that resource is closed. If you don't need this
  behavior you can explicitly supply `nil` for the resource. If you supply `nil` for the resource,
  however, you must manually set the `for` option, e.g: `for: Notifier` or `for: [Notifier1, Notifier2]`
  """
  defstruct [:resource, :domain, :action, :data, :changeset, :actor, :for, :from, metadata: %{}]

  @type t :: %__MODULE__{}

  def new(resource, opts) do
    struct(%__MODULE__{resource: resource}, opts)
  end
end
