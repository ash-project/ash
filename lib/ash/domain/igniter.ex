# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Domain.Igniter do
    @moduledoc "Codemods for working with Ash.Domain modules"

    @doc "List all domain modules found in the project"
    def list_domains(igniter) do
      # If any files have been removed, fall back to scanning all sources
      # since a removed file could have contained a domain definition
      if igniter.rms != [] do
        {igniter, scan_sources_for_domains(igniter, scan_all: true)}
      else
        case get_compiled_domains(igniter) do
          {:ok, compiled_domains} ->
            # Fast path: combine compiled domains with any changed sources
            changed_domains = scan_sources_for_domains(igniter, scan_all: false)
            {igniter, Enum.uniq(compiled_domains ++ changed_domains)}

          :error ->
            # Fallback: scan all sources if we can't get compiled domains
            {igniter, scan_sources_for_domains(igniter, scan_all: true)}
        end
      end
    end

    defp get_compiled_domains(igniter) do
      app_name = Igniter.Project.Application.app_name(igniter)
      domains = Application.get_env(app_name, :ash_domains, [])
      {:ok, domains}
    rescue
      _ -> :error
    end

    defp scan_sources_for_domains(igniter, opts) do
      Ash.Igniter.find_all_matching_modules(
        igniter,
        fn _module, zipper ->
          match?({:ok, _}, Igniter.Code.Module.move_to_use(zipper, Ash.Domain))
        end,
        opts
      )
    end

    @doc "Adds a resource reference to a domain's `resources` block"
    def add_resource_reference(igniter, domain, resource) do
      {igniter, domains} = Ash.Domain.Igniter.list_domains(igniter)

      if domain in domains do
        Igniter.Project.Module.find_and_update_module!(igniter, domain, fn zipper ->
          case Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :resources,
                 1
               ) do
            :error ->
              code =
                """
                resources do
                  resource #{inspect(resource)}
                end
                """

              {:ok, Igniter.Code.Common.add_code(zipper, code)}

            {:ok, zipper} ->
              with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper),
                   :error <-
                     Igniter.Code.Function.move_to_function_call_in_current_scope(
                       zipper,
                       :resource,
                       [1, 2],
                       &Igniter.Code.Function.argument_equals?(&1, 0, resource)
                     ) do
                {:ok, Igniter.Code.Common.add_code(zipper, "resource #{inspect(resource)}")}
              else
                _ ->
                  {:ok, zipper}
              end
          end
        end)
      else
        upgrade_plain_module_to_domain(igniter, domain, resource, fn app_name ->
          """
          use Ash.Domain,
            otp_app: :#{app_name}

          resources do
            resource #{inspect(resource)}
          end
          """
        end)
      end
    end

    @doc "Adds a code interface if not present to the given resource on the given domain"
    def add_new_code_interface(igniter, domain, resource, name, definition) do
      {igniter, domains} = Ash.Domain.Igniter.list_domains(igniter)

      if domain in domains do
        Igniter.Project.Module.find_and_update_module!(igniter, domain, fn zipper ->
          case Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :resources,
                 1
               ) do
            :error ->
              {:ok, zipper}

            {:ok, zipper} ->
              with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper),
                   {:ok, zipper} <-
                     Igniter.Code.Function.move_to_function_call_in_current_scope(
                       zipper,
                       :resource,
                       [1, 2],
                       &Igniter.Code.Function.argument_equals?(&1, 0, resource)
                     ) do
                cond do
                  Igniter.Code.Function.function_call?(zipper, :resource, 1) ->
                    {:ok,
                     Igniter.Code.Common.replace_code(
                       zipper,
                       Sourceror.parse_string!("""
                       resource #{inspect(resource)} do
                         #{definition}
                       end
                       """)
                     )}

                  Igniter.Code.Function.function_call?(zipper, :resource, 2) ->
                    with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
                      case Igniter.Code.Function.move_to_function_call_in_current_scope(
                             zipper,
                             :define,
                             [1, 2],
                             &Igniter.Code.Function.argument_equals?(&1, 0, name)
                           ) do
                        {:ok, _} ->
                          {:ok, zipper}

                        :error ->
                          {:ok,
                           Igniter.Code.Common.add_code(zipper, definition, placement: :after)}
                      end
                    end
                end
              else
                _ ->
                  {:ok, zipper}
              end
          end
        end)
      else
        upgrade_plain_module_to_domain(igniter, domain, resource, fn app_name ->
          """
          use Ash.Domain,
            otp_app: :#{app_name}

          resources do
            resource #{inspect(resource)} do
              #{definition}
            end
          end
          """
        end)
      end
    end

    # Shared helper: upgrades a plain module to an Ash.Domain when the module
    # exists as a file but does not yet `use Ash.Domain`. Emits a notice,
    # patches the module in-place using the caller-supplied code block,
    # and registers the domain in config.exs under :ash_domains.
    # Falls back to the original warning when the module does not exist at all.
    defp upgrade_plain_module_to_domain(igniter, domain, resource, code_fn) do
      {exists?, igniter} = Igniter.Project.Module.module_exists(igniter, domain)

      if exists? do
        app_name = Igniter.Project.Application.app_name(igniter)

        igniter
        |> Igniter.add_notice(
          "#{inspect(domain)} exists but is not an `Ash.Domain`. Adding `use Ash.Domain` to it."
        )
        |> Igniter.Project.Module.find_and_update_module!(domain, fn zipper ->
          {:ok, Igniter.Code.Common.add_code(zipper, code_fn.(app_name))}
        end)
        |> Igniter.Project.Config.configure(
          "config.exs",
          app_name,
          [:ash_domains],
          [domain],
          updater: fn list ->
            Igniter.Code.List.prepend_new_to_list(list, domain)
          end
        )
      else
        Igniter.add_warning(
          igniter,
          "Domain #{inspect(domain)} was not an `Ash.Domain`, so could not add `#{inspect(resource)}` to its resource list."
        )
      end
    end

    @doc "Removes a resource reference from a domain's `resources` block"
    def remove_resource_reference(igniter, domain, resource) do
      Igniter.Project.Module.find_and_update_module!(igniter, domain, fn zipper ->
        case Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :resources,
               1
             ) do
          :error ->
            zipper

          {:ok, zipper} ->
            with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper),
                 {:ok, zipper} <-
                   Igniter.Code.Function.move_to_function_call_in_current_scope(
                     zipper,
                     :resource,
                     1,
                     fn call ->
                       Igniter.Code.Function.argument_matches_predicate?(
                         call,
                         0,
                         &Igniter.Code.Common.nodes_equal?(&1, resource)
                       )
                     end
                   ) do
              {:ok, Sourceror.Zipper.remove(zipper)}
            else
              _ ->
                {:ok, zipper}
            end
        end
      end)
    end
  end
end
