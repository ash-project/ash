# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Igniter do
    @moduledoc "Codemods and utilities for working with Ash & Igniter"

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
