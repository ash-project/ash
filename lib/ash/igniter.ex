# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Igniter do
    @moduledoc "Codemods and utilities for working with Ash & Igniter"

    @doc """
    Returns a concise log of tasks in the igniter's task queue.

    Use this when an igniter run fails to show which tasks were queued and may not
    have run. Pass the igniter struct (e.g. from a rescue block or from before
    calling `run_with_failure_report/2`).

    ## Examples

        # In a rescue after Igniter.do_or_dry_run(igniter) fails:
        rescue
          e ->
            IO.puts(Ash.Igniter.format_pending_tasks(igniter))
            reraise e, __STACKTRACE__
        end
    """
    @spec format_pending_tasks(Igniter.t()) :: String.t()
    def format_pending_tasks(igniter) do
      lines =
        igniter
        |> pending_task_entries()
        |> Enum.map(&format_task_entry/1)

      if lines == [] do
        "No tasks were queued."
      else
        [
          "Tasks that did not run (or may not have completed):",
          "" | Enum.map(lines, fn line -> "  • " <> line end)
        ]
        |> Enum.join("\n")
      end
    end

    @doc """
    Returns a list of `{task_name, args}` for each queued task.

    Useful when you need to inspect or replay tasks programmatically.
    Delayed tasks are included with `args: [":delayed"]`.
    """
    @spec pending_task_entries(Igniter.t()) :: [{String.t(), [String.t()]}]
    def pending_task_entries(igniter) do
      Enum.map(igniter.tasks || [], fn
        name when is_binary(name) ->
          {name, []}

        {name, args} when is_list(args) ->
          {name, args}

        {name, args, :delayed} ->
          {name, args ++ [":delayed"]}
      end)
    end

    @doc """
    Runs the igniter with `Igniter.do_or_dry_run/2` and on failure prints a
    concise log of queued tasks that did not run, then re-raises.

    Options are passed through to `Igniter.do_or_dry_run/2`, except:

    - `:on_failure_log` - optional `(log_string :: String.t() -> term())` callback
      invoked with the formatted pending-tasks log when a failure occurs (e.g. for
      tests). Default is to write the log to stderr.
    """
    @spec run_with_failure_report(Igniter.t(), keyword()) ::
            Igniter.t()
            | :changes_aborted
            | :changes_made
            | :dry_run_with_changes
            | :dry_run_with_no_changes
            | :issues
            | :no_changes
            | nil
    def run_with_failure_report(igniter, opts \\ []) do
      run_opts = Keyword.drop(opts, [:on_failure_log])

      Igniter.do_or_dry_run(igniter, run_opts)
    rescue
      e ->
        log_fn = Keyword.get(opts, :on_failure_log, &IO.puts(:stderr, &1))
        log_fn.("\n" <> format_pending_tasks(igniter) <> "\n")
        reraise e, __STACKTRACE__
    end

    defp format_task_entry({name, []}), do: name
    defp format_task_entry({name, args}), do: name <> " " <> Enum.join(args, " ")

    @doc "Adds a codegen task, or updates the name to be `<old_name>_and_name`"
    def codegen(igniter, name) do
      has_codegen? =
        Enum.any?(igniter.tasks, fn
          {"ash.codegen", _args} ->
            true

          _ ->
            false
        end)

      if has_codegen? do
        Map.update!(igniter, :tasks, fn tasks ->
          Enum.map(tasks, fn
            {"ash.codegen", [old_name | rest]} ->
              {"ash.codegen", [old_name <> "_and_#{name}" | rest]}

            task ->
              task
          end)
        end)
      else
        Igniter.add_task(igniter, "ash.codegen", [name])
      end
    end

    @doc false
    def find_all_matching_modules(igniter, predicate, opts \\ []) do
      if opts[:scan_all] do
        scan_all_sources(igniter, predicate)
      else
        scan_changed_sources_only(igniter, predicate)
      end
    end

    defp scan_changed_sources_only(igniter, predicate) do
      sources =
        igniter.rewrite.sources
        |> Map.values()
        |> Enum.filter(&Igniter.changed?/1)

      scan_sources_for_modules(sources, predicate)
    end

    defp scan_all_sources(igniter, predicate) do
      igniter = Igniter.include_all_elixir_files(igniter)
      sources = Rewrite.sources(igniter.rewrite)
      scan_sources_for_modules(sources, predicate)
    end

    defp scan_sources_for_modules(sources, predicate) do
      sources
      |> Enum.filter(&match?(%Rewrite.Source{filetype: %Rewrite.Source.Ex{}}, &1))
      |> Task.async_stream(
        fn source ->
          source
          |> Rewrite.Source.get(:quoted)
          |> Sourceror.Zipper.zip()
          |> Sourceror.Zipper.traverse([], fn zipper, acc ->
            case zipper.node do
              {:defmodule, _, [_, _]} ->
                {:ok, mod_zipper} = Igniter.Code.Function.move_to_nth_argument(zipper, 0)

                module_name =
                  mod_zipper
                  |> Igniter.Code.Common.expand_alias()
                  |> Sourceror.Zipper.node()
                  |> Igniter.Project.Module.to_module_name()

                with module_name when not is_nil(module_name) <- module_name,
                     {:ok, do_zipper} <- Igniter.Code.Common.move_to_do_block(zipper),
                     true <- predicate.(module_name, do_zipper) do
                  {zipper, [module_name | acc]}
                else
                  _ ->
                    {zipper, acc}
                end

              _ ->
                {zipper, acc}
            end
          end)
          |> elem(1)
        end,
        timeout: :infinity
      )
      |> Enum.flat_map(fn {:ok, v} -> v end)
      |> Enum.uniq()
    end
  end
end
