# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Argument.Build, for: Ash.Reactor.Dsl.NotificationMetadata do
  import Reactor.Template, only: [is_template: 1]

  @doc false
  @impl true
  def build(notification_metadata) when is_template(notification_metadata.source) do
    %Reactor.Argument{
      name: :notification_metadata,
      source: notification_metadata.source,
      transform: notification_metadata.transform
    }
    |> then(&{:ok, [&1]})
  end

  def build(notification_metadata) when is_map(notification_metadata.source) do
    Reactor.Argument.from_value(
      :notification_metadata,
      notification_metadata.source,
      transform: notification_metadata.transform
    )
    |> then(&{:ok, [&1]})
  end

  def build(notification_metadata) when is_nil(notification_metadata.source) do
    build(%{notification_metadata | source: %{}})
  end
end
