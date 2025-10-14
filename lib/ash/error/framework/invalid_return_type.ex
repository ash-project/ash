# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.InvalidReturnType do
  @moduledoc "Used when a callback returns an invalid type"

  use Splode.Error, fields: [:message], class: :framework

  def message(%{message: message}) do
    message
  end
end
