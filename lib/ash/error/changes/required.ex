# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Changes.Required do
  @moduledoc "Used when an attribute or relationship is required"

  use Splode.Error, fields: [:field, :type, :resource], class: :invalid

  def message(error) do
    "#{error.type} #{error.field} is required"
  end
end
