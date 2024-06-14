defmodule Ash.Domain.Igniter do
  def add_resource_reference(igniter, domain, resource) do
    igniter
    |> Igniter.update_elixir_file(Igniter.Code.Module.proper_location(domain), fn zipper ->
      case Igniter.Code.Module.move_to_module_using(zipper, Ash.Domain) do
        :error ->
          {:error, "Could not find module using Ash.Domain"}

        {:ok, zipper} ->
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

              Igniter.Code.Common.add_code(zipper, code)

            {:ok, zipper} ->
              with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
                case Igniter.Code.Function.move_to_function_call_in_current_scope(
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
                  {:ok, _} ->
                    zipper

                  :error ->
                    Igniter.Code.Common.add_code(zipper, "resource #{inspect(resource)}")
                end
              else
                _ ->
                  zipper
              end
          end
      end
    end)
  end

  def remove_resource_reference(igniter, domain, resource) do
    igniter
    |> Igniter.update_elixir_file(Igniter.Code.Module.proper_location(domain), fn zipper ->
      case Igniter.Code.Module.move_to_module_using(zipper, Ash.Domain) do
        :error ->
          {:error, "Could not find module using Ash.Domain"}

        {:ok, zipper} ->
          case Igniter.Code.Function.move_to_function_call_in_current_scope(
                 zipper,
                 :resources,
                 1
               ) do
            :error ->
              zipper

            {:ok, zipper} ->
              with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
                case Igniter.Code.Function.move_to_function_call_in_current_scope(
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
                  {:ok, zipper} ->
                    Sourceror.Zipper.remove(zipper)

                  :error ->
                    zipper
                end
              else
                _ ->
                  zipper
              end
          end
      end
    end)
  end
end
