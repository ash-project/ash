# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.Resource do
  @moduledoc "This check is true when the resource matches the provided resource name or names."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    {operator, resource} =
      case options[:resource] do
        [resource] -> {"==", resource}
        resources -> {"in", resources}
      end

    "resource #{operator} #{resource}"
  end

  @impl true
  def match?(_actor, %{resource: resource}, options) do
    resource in options[:resource]
  end
end
