# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.Matches do
  @moduledoc "This check is true when the specified function returns true"
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    options[:description]
  end

  @impl true
  def match?(actor, request, options) do
    options[:func].(actor, request)
  end
end
