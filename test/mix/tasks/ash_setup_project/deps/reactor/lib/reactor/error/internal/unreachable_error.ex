# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Internal.UnreachableError do
  @moduledoc """
  An error that should never happen.
  """

  use Reactor.Error,
    fields: [:bindings, :message, :file, :line],
    class: :reactor

  @doc false
  @impl true
  def message(error) do
    [
      """
      # Unreachable Error

      You should _never_ see this error in the wild. If you do please raise an issue on
      the Reactor repository:

          https://github.com/ash-project/reactor/issues/new

      And paste the following information:

      --- BEGIN COPY ---

      Reached unreachable code at #{error.file}:#{error.line}:

      #{error.message}
      """
    ]
    |> maybe_append(maybe_format_bindings(error))
    |> maybe_append(maybe_format_stacktrace(error))
    |> maybe_append(format_system_info())
    |> maybe_append(format_running_applications())
    |> Enum.join("\n")
  end

  @doc """
  Create an unreachable error.
  """
  @spec unreachable(String.t()) :: Macro.output()
  defmacro unreachable(message) do
    quote do
      unquote(__MODULE__).exception(
        bindings: binding(),
        line: __ENV__.line,
        file: __ENV__.file,
        message: unquote(message)
      )
    end
  end

  @doc """
  Bang version of `unreachable/1`.
  """
  @spec unreachable!(String.t()) :: Macro.output()
  defmacro unreachable!(message) do
    quote do
      raise unquote(__MODULE__).exception(
              bindings: binding(),
              line: __ENV__.line,
              file: __ENV__.file,
              message: unquote(message)
            )
    end
  end

  defp maybe_format_bindings(error) do
    if Enum.any?(error.bindings) do
      bindings =
        error.bindings
        |> Enum.map_join("\n", fn {name, value} ->
          "  - `#{inspect(name)}`: `#{inspect(value)}`"
        end)

      """
      Bindings:

      #{bindings}
      """
    end
  end

  defp maybe_format_stacktrace(error) do
    if error.stacktrace do
      stacktrace =
        error.stacktrace.stacktrace
        |> Enum.drop(2)
        |> Exception.format_stacktrace()

      """
      Backtrace:

      #{stacktrace}
      """
    end
  end

  # sobelow_skip ["Traversal.FileModule"]
  defp format_system_info do
    elixir = System.build_info()

    erlang_vsn =
      [
        :code.root_dir(),
        "releases",
        :erlang.system_info(:otp_release),
        "OTP_VERSION"
      ]
      |> Path.join()
      |> File.read!()
      |> String.trim()

    system =
      with {_, code} when code > 0 <- System.cmd("uname", ["-a"]),
           {_, code} when code > 0 <- System.cmd("ver", []) do
        {family, name} = :os.type()

        version =
          case :os.version() do
            version when is_tuple(version) ->
              version
              |> Tuple.to_list()
              |> Enum.map_join(".", &to_string/1)

            version when is_list(version) ->
              to_string(version)
          end

        "#{name} #{family} / #{version}"
      else
        {uname, 0} -> uname
      end

    """
    System:

      Elixir #{elixir["version"]} (#{elixir[:revision]}) compiled with Erlang/OTP #{elixir[:otp_release]}
      Erlang/OTP #{erlang_vsn} [erts-#{:erlang.system_info(:version)}]
      #{system}
    """
  end

  defp format_running_applications do
    applications =
      Application.loaded_applications()
      |> Enum.map_join("\n", fn {app, _, vsn} ->
        "  - #{app} #{vsn}"
      end)

    """
    Running applications:

    #{applications}

    --- END COPY ---

    Please carefully read all of the above and redact any sensitive information.
    """
  end
end
