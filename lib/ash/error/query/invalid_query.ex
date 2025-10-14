# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.InvalidQuery do
  @moduledoc """
  A generic error that can be used to add an error to a query for a specific field
  """

  use Splode.Error, fields: [:field, :fields, :message, :value], class: :invalid

  def message(error) do
    error.message
  end
end
