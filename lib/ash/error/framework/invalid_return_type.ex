# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.InvalidReturnType do
  @moduledoc """
  Raised when a behaviour implementation returns a value that does not match
  the behaviour's callback return type.

  The `message` field should identify the behaviour module, the callback
  (e.g. `Module.function/arity`), and list the allowed return shapes so
  implementors can fix the return value.

  ## Usage in behaviour wrappers

  Wrappers should call the implementation and then either match the result
  against the allowed shapes or use `Ash.BehaviourHelpers.call_and_validate_return/5`.
  When the result does not match any allowed shape, raise this error with a
  message in the format above (behaviour, callback, and allowed shapes).
  """

  use Splode.Error, fields: [:message], class: :framework

  def message(%{message: message}) do
    message
  end
end
