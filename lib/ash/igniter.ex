# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Igniter do
    @moduledoc "Codemods and utilities for working with Ash & Igniter"

    require Logger

    # Error structures for error handling
    defmodule Error do
      defexception [:type, :location, :context, :message]

      def exception(opts) do
        %__MODULE__{
          type: opts[:type],
          location: opts[:location],
          context: opts[:context],
          message: opts[:message] || "Error occurred"
        }
      end
    end

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
      case with_safe_rewrite_access(igniter, fn rewrite ->
             sources =
               rewrite.sources
               |> Map.values()
               |> Enum.filter(&Igniter.changed?/1)

             scan_sources_for_modules(sources, predicate)
           end) do
        result when is_list(result) -> result
        {:error, _reason} -> []
      end
    end

    defp scan_all_sources(igniter, predicate) do
      case with_safe_igniter_update(igniter, fn updated_igniter ->
             case with_safe_rewrite_access(updated_igniter, fn rewrite ->
                    sources = Rewrite.sources(rewrite)
                    scan_sources_for_modules(sources, predicate)
                  end) do
               result when is_list(result) -> result
               {:error, _reason} -> []
             end
           end) do
        result when is_list(result) -> result
        {:error, _reason} -> []
      end
    end

    # Error handling wrapper for safe rewrite access
    defp with_safe_rewrite_access(igniter, fun) do
      case igniter do
        %{rewrite: %{sources: _} = rewrite} ->
          try do
            fun.(rewrite)
          rescue
            e ->
              Logger.error("Error accessing rewrite.sources",
                error: inspect(e),
                file: __ENV__.file,
                line: __ENV__.line
              )

              {:error, {:rewrite_access_error, e}}
          end

        %{rewrite: nil} ->
          Logger.error("igniter.rewrite is nil",
            file: __ENV__.file,
            line: __ENV__.line
          )

          {:error, {:nil_rewrite, "igniter.rewrite is nil"}}

        _ ->
          Logger.error("Invalid igniter structure",
            file: __ENV__.file,
            line: __ENV__.line
          )

          {:error, {:invalid_structure, "igniter does not have expected structure"}}
      end
    end

    # Error handling wrapper for safe igniter update
    defp with_safe_igniter_update(igniter, fun) do
      try do
        updated_igniter = Igniter.include_all_elixir_files(igniter)
        fun.(updated_igniter)
      rescue
        e ->
          Logger.error("Error updating igniter with include_all_elixir_files",
            error: inspect(e),
            file: __ENV__.file,
            line: __ENV__.line
          )

          {:error, {:igniter_update_error, e}}
      end
    end

    defp scan_sources_for_modules(sources, predicate) do
      sources
      |> Enum.filter(&match?(%Rewrite.Source{filetype: %Rewrite.Source.Ex{}}, &1))
      |> Task.async_stream(
        fn source ->
          with_safe_source_processing(source, predicate)
        end,
        timeout: :infinity
      )
      |> Enum.flat_map(fn
        {:ok, {:ok, v}} -> v
        {:ok, {:error, reason}} ->
          Logger.warning("Error processing source",
            error: inspect(reason),
            file: __ENV__.file,
            line: __ENV__.line
          )

          []

        {:error, reason} ->
          Logger.error("Task.async_stream error",
            error: inspect(reason),
            file: __ENV__.file,
            line: __ENV__.line
          )

          []
      end)
      |> Enum.uniq()
    end

    # Error handling wrapper for safe source processing
    defp with_safe_source_processing(source, predicate) do
      try do
        quoted = Rewrite.Source.get(source, :quoted)

        if is_nil(quoted) do
          Logger.warning("Rewrite.Source.get(:quoted) returned nil",
            file: __ENV__.file,
            line: __ENV__.line
          )

          {:error, []}
        else
          zipper = Sourceror.Zipper.zip(quoted)

          result =
            zipper
            |> Sourceror.Zipper.traverse([], fn zipper, acc ->
              case zipper.node do
                {:defmodule, _, [_, _]} ->
                  with_safe_module_extraction(zipper, predicate, acc)

                _ ->
                  {zipper, acc}
              end
            end)
            |> elem(1)

          {:ok, result}
        end
      rescue
        e ->
          Logger.error("Error processing source",
            error: inspect(e),
            file: __ENV__.file,
            line: __ENV__.line
          )

          {:error, []}
      end
    end

    # Error handling wrapper for safe module extraction
    defp with_safe_module_extraction(zipper, predicate, acc) do
      case Igniter.Code.Function.move_to_nth_argument(zipper, 0) do
        {:ok, mod_zipper} ->
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

        :error ->
          Logger.warning("Error moving to nth argument",
            file: __ENV__.file,
            line: __ENV__.line
          )

          {zipper, acc}
      end
    end
  end
end
