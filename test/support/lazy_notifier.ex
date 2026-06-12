# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.LazyNotifier do
  @moduledoc """
  Notifier used to test that `load/2` dependencies are honored even when the
  notifier module has not been loaded yet. Lives in test/support (compiled to
  disk) so tests can `:code.delete/1` it and the VM can load it again.
  """
  use Ash.Notifier

  def load(_resource, _action), do: [:shout]

  def notify(notification) do
    send(
      Application.get_env(Ash.Test.Notifier.PubSubTest.PubSub, :notifier_test_pid),
      {:lazy_notify, notification}
    )
  end
end
