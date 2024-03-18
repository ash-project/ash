defmodule Ash.Error.Framework.FlagAssertionFailed do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Ash.Error.Exception

  use Splode.Error, fields: [:flag, :heading], class: :framework

  def message(error) do
    flag_env_name =
      error.flag
      |> to_string()
      |> String.trim_trailing("?")
      |> String.upcase()

    """
    #{error.heading}

    If you are trying to develop or test against a flagged feature either set the flag to the appropriate value in `config.exs` or set the `FLAG_#{flag_env_name}` environment variable (at both compile time and run time).
    """
  end
end
