# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Warning do
  @moduledoc false

  @type location :: :erl_anno.anno()
  @type stacktrace :: Exception.stacktrace() | Macro.Env.t()
  @type warning_message :: String.t() | term()
  @type warning_item :: warning_message() | {warning_message(), location()}

  @doc """
  Emits a warning with optional location information.

  ## Parameters

  - `message` - The warning message (string or any term)
  - `location` - Optional erl_anno location information
  - `stacktrace` - Optional stacktrace (defaults to current stacktrace)

  ## Examples

      # Simple warning without location
      Spark.Warning.warn("This is deprecated", nil, __ENV__)

      # Warning with location annotation
      Spark.Warning.warn("Invalid configuration", location, __ENV__)

      # Warning with custom stacktrace
      Spark.Warning.warn("Something went wrong", nil, custom_stacktrace)
  """
  @spec warn(warning_message(), location() | nil, stacktrace() | nil) :: :ok
  def warn(message, location \\ nil, stacktrace \\ nil)

  def warn(message, location, stacktrace) when is_binary(message) do
    warn_context =
      if is_nil(location) do
        stacktrace || get_filtered_stacktrace()
      else
        warn_context =
          case :erl_anno.file(location) do
            :undefined -> [file: "unknown_file"]
            file -> [file: Path.relative_to_cwd(to_string(file))]
          end

        case :erl_anno.location(location) do
          {line, column} -> Keyword.merge(warn_context, line: line, column: column)
          line -> Keyword.merge(warn_context, line: line)
        end
      end

    IO.warn(message, warn_context)
  end

  def warn(message, location, stacktrace) do
    warn(inspect(message), location, stacktrace)
  end

  @doc """
  Emits a deprecation warning with optional location information.

  ## Parameters

  - `item` - The deprecated item (string)
  - `message` - Additional deprecation message (optional)
  - `location` - Optional erl_anno location information
  - `stacktrace` - Optional stacktrace (defaults to current stacktrace)

  ## Examples

      Spark.Warning.warn_deprecated("old_option")
      Spark.Warning.warn_deprecated("old_option", "Use new_option instead", location)
  """
  @spec warn_deprecated(String.t(), String.t() | nil, location() | nil, stacktrace() | nil) :: :ok
  def warn_deprecated(item, message \\ nil, location \\ nil, stacktrace \\ nil) do
    base_message = "#{item} is deprecated"

    full_message =
      if message do
        base_message <> ". " <> message
      else
        base_message
      end

    warn(full_message, location, stacktrace)
  end

  @spec get_filtered_stacktrace() :: [term()]
  defp get_filtered_stacktrace do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    # Remove the latest 2 frames to exclude this Warning module's calls
    stacktrace |> Enum.drop(3)
  end
end
