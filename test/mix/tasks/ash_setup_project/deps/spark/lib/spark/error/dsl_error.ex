# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Error.DslError do
  @moduledoc "Used when a DSL is incorrectly configured."
  @attrs [:module, :message, :path, :stacktrace, :location]
  defexception @attrs

  @type t :: %__MODULE__{
          __exception__: true,
          module: nil | module,
          message: String.t() | any,
          path: [:atom],
          stacktrace: any,
          location: :erl_anno.anno() | nil
        }

  defmodule Stacktrace do
    @moduledoc false
    defstruct [:stacktrace]

    defimpl Inspect do
      def inspect(_, _) do
        "%Stacktrace{}"
      end
    end
  end

  @impl true
  def exception(message) when is_binary(message), do: exception(message: message)

  def exception(opts) do
    {:current_stacktrace, stacktrace} =
      Process.info(self(), :current_stacktrace)

    opts =
      opts
      |> Enum.to_list()
      |> Keyword.put(:stacktrace, %Stacktrace{stacktrace: stacktrace})
      |> Keyword.take(@attrs)

    struct!(__MODULE__, opts)
  end

  @impl true
  def message(%{module: module, message: message, path: blank, location: location})
      when is_nil(blank) or blank == [] do
    "#{module_line(module)}#{get_message(message)}#{get_location(location)}"
  end

  def message(%{module: module, message: message, path: dsl_path, location: location}) do
    dsl_path =
      Enum.map_join(dsl_path, " -> ", fn item ->
        try do
          to_string(item)
        rescue
          _ ->
            inspect(item)
        end
      end)

    "#{module_line(module)}#{dsl_path} #{get_location(location)}:\n  #{get_message(message)}"
  end

  defp get_location(location)
  defp get_location(nil), do: ""

  defp get_location(location) do
    file =
      case :erl_anno.file(location) do
        :undefined -> "unknown_file"
        file -> Path.relative_to_cwd(to_string(file))
      end

    case :erl_anno.location(location) do
      {line, column} -> " defined in #{Exception.format_file_line_column(file, line, column)}"
      line -> "defined in #{Exception.format_file_line(file, line)}"
    end
  end

  defp get_message(message) when is_exception(message) do
    Exception.format(:error, message)
  end

  defp get_message(message) when is_binary(message) do
    message
  end

  defp get_message(message) do
    inspect(message)
  end

  defp module_line(nil), do: ""

  defp module_line(module) do
    "[#{normalize_module_name(module)}]\n"
  end

  defp normalize_module_name(module) do
    inspect(module)
  end
end
