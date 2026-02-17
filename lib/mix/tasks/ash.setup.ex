# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.Setup do
  @moduledoc """
  Runs all setup tasks for any extension on any resource/domain in your application.

  When a failure occurs, the task compiles a concise failure record (errors, context, location)
  and raises with a clear message. Use `run_with_failure_record/1` to get the record without
  raising.
  """
  use Mix.Task

  @shortdoc "Runs all setup tasks for any extension on any resource/domain in your application."
  @doc @shortdoc
  def run(argv) do
    case run_with_failure_record(argv) do
      :ok ->
        :ok

      {:error, records} ->
        message = format_failure_records(records)
        Mix.raise(message)
    end
  end

  @doc """
  Runs setup and returns either `:ok` or `{:error, [failure_record]}`.

  Use this when you need the structured failure record (error, context, location)
  without raising. Each failure record is a map with:
  - `:error` – exception or error term
  - `:context` – short description of what was being done
  - `:location` – code/phase identifier (e.g. "compile", "extensions!", "extension.setup")
  """
  def run_with_failure_record(argv) do
    records = []

    # Step 1: compile
    records =
      try do
        Mix.Task.run("compile")
        records
      rescue
        e ->
          [
            %{
              error: e,
              context:
                "Mix.Task.run(\"compile\") – codebase/extension compile or load failed",
              location: "Mix.Tasks.Ash.Setup.run/1 → Mix.Task.run(\"compile\")"
            }
            | records
          ]
      end

    if records != [] do
      {:error, Enum.reverse(records)}
    else
      # Step 2: extensions!()
      {records, extensions} =
        try do
          exts = Ash.Mix.Tasks.Helpers.extensions!(argv)
          {records, exts}
        rescue
          e ->
            {[
               %{
                 error: e,
                 context:
                   "Ash.Mix.Tasks.Helpers.extensions!(argv) – failed to return list of extension modules (e.g. option parse, domains, load, or async_stream)",
                 location: "Mix.Tasks.Ash.Setup.run/1 → extensions!(argv)"
               }
               | records
             ], []}
        end

      if records != [] do
        {:error, Enum.reverse(records)}
      else
        # Step 3: Enum.map over extensions – per-extension name/setup failures
        extension_records =
          Enum.flat_map(extensions, fn extension ->
            run_extension_setup(extension, argv)
          end)

        if extension_records == [] do
          :ok
        else
          {:error, extension_records}
        end
      end
    end
  end

  defp run_extension_setup(extension, argv) do
    if not function_exported?(extension, :setup, 1) do
      []
    else
      run_extension_setup_impl(extension, argv)
    end
  end

  defp run_extension_setup_impl(extension, argv) do
    {name, name_records} =
      try do
        n =
          if function_exported?(extension, :name, 0) do
            extension.name()
          else
            inspect(extension)
          end
        {n, []}
      rescue
        e ->
          {inspect(extension),
           [
             %{
               error: e,
               context: "extension.name/0 or inspect(extension) for display name",
               location:
                 "Mix.Tasks.Ash.Setup.run/1 → Enum.map(extension) → name for #{inspect(extension)}"
             }
           ]}
      end

    setup_records =
      try do
        Mix.shell().info("Running setup for #{name}...")
        extension.setup(argv)
        []
      rescue
        e ->
          [
            %{
              error: e,
              context: "extension.setup(argv) – setup for #{name}",
              location:
                "Mix.Tasks.Ash.Setup.run/1 → Enum.map(extension) → extension.setup(argv) [#{inspect(extension)}]"
            }
          ]
      end

    name_records ++ setup_records
  end

  defp format_failure_records(records) do
    lines =
      Enum.map(records, fn %{error: e, context: ctx, location: loc} ->
        ["  [", loc, "] ", ctx, "\n    error: ", format_error(e)]
      end)

    "Ash.Setup failed:\n" <> IO.iodata_to_binary(lines)
  end

  defp format_error(%{__struct__: struct} = e) when is_struct(e) do
    inspect(struct) <> " " <> Exception.message(e)
  end
  defp format_error(e), do: inspect(e)
end
