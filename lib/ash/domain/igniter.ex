if Code.ensure_loaded?(Igniter) do
  defmodule Ash.Domain.Igniter do
    @moduledoc "Codemods for working with Ash.Domain modules"

    @doc "List all domain modules found in the project"
    def list_domains(igniter) do
      Igniter.Project.Module.find_all_matching_modules(igniter, fn _mod, zipper ->
        zipper
        |> Igniter.Code.Module.move_to_use(Ash.Domain)
        |> case do
          {:ok, _} ->
            true

          _ ->
            false
        end
      end)
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
        igniter
        |> Igniter.add_warning(
          "Domain #{inspect(domain)} was not an `Ash.Domain`, so could not add `#{inspect(resource)}` to its resource list."
        )
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
        igniter
        |> Igniter.add_warning(
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
