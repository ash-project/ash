# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.NotTemporalSafe do
  @moduledoc """
  Used when a change, validation or preparation that has not declared itself temporal
  safe is run as part of an action on a temporal resource.
  """

  use Splode.Error, fields: [:resource, :action, :module, :type], class: :framework

  def message(error) do
    """
    #{inspect(error.module)} is not temporal safe, but #{describe_action(error)} runs on a temporal resource.

    Every action on a temporal resource runs "as of" a point in time, which may be in the
    past or in the future. Only #{plural(error.type)} that declare themselves temporal safe
    may run there: one that never reads the wall clock (use `now()` in expressions, or the
    subject's `as_of`), has no side effects that assume the write is happening now, and
    performs any reads or nested actions through Ash so that `as_of` is threaded to them.

    If #{inspect(error.module)} meets that bar, declare it:

        @impl true
        def temporal_safe?(_opts), do: true

    If it is only safe with certain options, inspect them and return `false` otherwise.

    Anonymous function #{plural(error.type)} (and the `before_action`/`after_action`-style
    builtins that wrap one) are never temporal safe, because their safety cannot be known.
    Move the logic into a module #{to_string(error.type)} that declares `temporal_safe?/1`.

    See the temporal resources guide for more:
    https://hexdocs.pm/ash/temporal-resources.html
    """
  end

  defp describe_action(%{resource: resource, action: nil}),
    do: "an action on #{inspect(resource)}"

  defp describe_action(%{resource: resource, action: action}),
    do: "#{inspect(resource)}.#{action_name(action)}"

  defp action_name(%{name: name}), do: name
  defp action_name(name), do: name

  defp plural(:change), do: "changes"
  defp plural(:validation), do: "validations"
  defp plural(:preparation), do: "preparations"
  defp plural(other), do: "#{other}s"
end
