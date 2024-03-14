defmodule Ash.Error.Unknown.UnknownError do
  @moduledoc "Used when an unknown error occurs"
  use Ash.Error.Exception

  use Splode.Error, fields: [:error, :field], class: :unknown

  def splode_message(%{error: error}) do
    if is_binary(error) do
      to_string(error)
    else
      inspect(error)
    end
  end
end
