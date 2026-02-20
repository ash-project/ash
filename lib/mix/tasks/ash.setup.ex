# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Setup do
  @moduledoc """
  Runs all setup tasks for any extension on any resource/domain in your application.

  Failures are collected into a concise record (error, context, location). Use
  `run_with_failure_record/1` for programmatic access to failure records.
  """
  use Mix.Task

  @shortdoc "Runs all setup tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    case run_with_failure_record(argv) do
      :ok ->
        :ok

      {:error, records} when is_list(records) ->
        Enum.each(records, &print_failure_record/1)
        Mix.raise("ash.setup failed with #{length(records)} error(s). See above.")
    end
  end

  @doc """
  Runs setup and returns either `:ok` or `{:error, [failure_record]}`.

  Use this when you need programmatic access to failure details (error, context, location).
  """
  @type failure_record :: %{error: term(), context: map(), location: String.t()}

  @spec run_with_failure_record([String.t()]) :: :ok | {:error, [failure_record()]}
  def run_with_failure_record(argv) do
    failures = run_under_watch(argv)
    if failures == [], do: :ok, else: {:error, Enum.reverse(failures)}
  end

  defp run_under_watch(argv) do
    run_under_watch_compile(argv) ++
      run_under_watch_extensions(argv)
  end

  defp run_under_watch_compile(argv) do
    try do
      Mix.Task.run("compile")
      []
    rescue
      e ->
        [
          %{
            error: e,
            context: %{step: :compile, argv: argv},
            location: location_from_stacktrace(__STACKTRACE__)
          }
        ]
    catch
      kind, value ->
        [
          %{
            error: {kind, value},
            context: %{step: :compile, argv: argv},
            location: location_from_stacktrace(__STACKTRACE__)
          }
        ]
    end
  end

  defp run_under_watch_extensions(argv) do
    try do
      extensions = Ash.Mix.Tasks.Helpers.extensions!(argv)
      Enum.flat_map(extensions, &run_extension_setup(&1, argv))
    rescue
      e ->
        [
          %{
            error: e,
            context: %{step: :extensions!, argv: argv},
            location: location_from_stacktrace(__STACKTRACE__)
          }
        ]
    catch
      kind, value ->
        [
          %{
            error: {kind, value},
            context: %{step: :extensions!, argv: argv},
            location: location_from_stacktrace(__STACKTRACE__)
          }
        ]
    end
  end

  defp run_extension_setup(extension, argv) do
    try do
      if function_exported?(extension, :setup, 1) do
        name = get_extension_name(extension)
        Mix.shell().info("Running setup for #{name}...")
        extension.setup(argv)
      end

      []
    rescue
      e ->
        [
          record_extension_failure(extension, argv, e, __STACKTRACE__)
        ]
    catch
      kind, value ->
        [
          record_extension_failure(extension, argv, {kind, value}, __STACKTRACE__)
        ]
    end
  end

  defp get_extension_name(extension) do
    if function_exported?(extension, :name, 0) do
      extension.name()
    else
      inspect(extension)
    end
  end

  defp record_extension_failure(extension, argv, error, stacktrace) do
    phase =
      cond do
        is_exception_in?(stacktrace, Kernel, :function_exported?, 3) -> :check_setup_export
        is_exception_in?(stacktrace, extension, :name, 0) -> :get_name
        is_exception_in?(stacktrace, Mix.shell(), :info, 1) -> :shell_info
        is_exception_in?(stacktrace, extension, :setup, 1) -> :setup
        true -> :extension_setup
      end

    %{
      error: error,
      context: %{extension: extension, step: :setup, phase: phase, argv: argv},
      location: location_from_stacktrace(stacktrace)
    }
  end

  defp is_exception_in?(stacktrace, module, fun, arity) do
    Enum.any?(stacktrace, fn
      {^module, ^fun, ^arity, _} -> true
      {^module, ^fun, _, _} when is_list(arity) -> true
      _ -> false
    end)
  end

  defp location_from_stacktrace(stacktrace) do
    case Enum.find(stacktrace, fn
           {_, _, _, _} -> true
           {_, _, _} -> true
           _ -> false
         end) do
      {mod, fun, arity, [file: file, line: line]} when is_integer(arity) ->
        "#{file}:#{line} #{inspect(mod)}.#{fun}/#{arity}"

      {mod, fun, arity, [file: file, line: line]} when is_list(arity) ->
        "#{file}:#{line} #{inspect(mod)}.#{fun}/#{length(arity)}"

      {mod, fun, arity, _} when is_integer(arity) ->
        "#{inspect(mod)}.#{fun}/#{arity}"

      {mod, fun, arity, _} when is_list(arity) ->
        "#{inspect(mod)}.#{fun}/#{length(arity)}"

      {mod, fun, arity} when is_integer(arity) ->
        "#{inspect(mod)}.#{fun}/#{arity}"

      {mod, fun, arity} when is_list(arity) ->
        "#{inspect(mod)}.#{fun}/#{length(arity)}"

      _ ->
        "unknown"
    end
  end

  defp print_failure_record(record) do
    Mix.shell().error("[ash.setup] #{record.location}")
    Mix.shell().error("  context: #{inspect(record.context)}")
    Mix.shell().error("  error: #{format_error(record.error)}")
  end

  defp format_error(%{__struct__: _} = e), do: Exception.format(:error, e)
  defp format_error({kind, value}), do: "#{kind}: #{inspect(value)}"
  defp format_error(other), do: inspect(other)
end
