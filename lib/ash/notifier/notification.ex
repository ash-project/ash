defmodule Ash.Notifier.Notification do
  @moduledoc """
  Represents a notification that will be handled by a resource's notifiers
  """
  defstruct [:resource, :action, :data, :changeset, :actor]

  @type t :: %__MODULE__{}
end
