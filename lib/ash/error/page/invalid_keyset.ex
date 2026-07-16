# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Page.InvalidKeyset do
  @moduledoc "Used when a value is provided for a keyset that cannot be Base64 decoded."

  use Splode.Error, fields: [:value, :key], class: :invalid

  def message(%{value: value, key: nil}) do
    "Invalid value provided as a keyset: #{inspect(maybe_redact(value))}"
  end

  def message(%{value: value, key: key}) do
    "Invalid value provided as a keyset for #{to_string(key)}: #{inspect(maybe_redact(value))}"
  end

  # a keyset encodes the record's sort values and primary key, and is reversible
  # with `Base.decode64/1` + `:erlang.binary_to_term/1`, so the message is redacted
  # as a whole. The offending value remains on the `value` field of the error.
  defp maybe_redact(value) do
    if Application.get_env(:ash, :redact_sensitive_values_in_errors?, false) do
      Ash.Helpers.redact(value)
    else
      value
    end
  end
end
