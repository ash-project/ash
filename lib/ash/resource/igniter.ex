defmodule Ash.Resource.Igniter do
  @moduledoc "Codemods for working with Ash.Resource modules"

  def list_resources(igniter) do
    app_name = Igniter.Project.Application.app_name()

    resources =
      [Ash.Resource | List.wrap(Application.get_env(app_name, :base_resources))]

    Igniter.Code.Module.find_all_matching_modules(igniter, fn _mod, zipper ->
      Enum.any?(resources, fn resource ->
        zipper
        |> Igniter.Code.Module.move_to_using(resource)
        |> case do
          {:ok, _} ->
            true

          _ ->
            false
        end
      end)
    end)
  end

  def add_attribute(igniter, resource, attribute) do
    Igniter.Code.Module.find_and_update_module!(igniter, resource, fn zipper ->
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :attributes,
               1
             ),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        Igniter.Code.Common.add_code(zipper, attribute)
      else
        _ ->
          attributes_with_attribute = """
          attributes do
            #{attribute}
          end
          """

          Igniter.Code.Common.add_code(zipper, attributes_with_attribute)
      end
    end)
  end

  def add_action(igniter, resource, action) do
    Igniter.Code.Module.find_and_update_module!(igniter, resource, fn zipper ->
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, :actions, 1),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        Igniter.Code.Common.add_code(zipper, action)
      else
        _ ->
          actions_with_action = """
          actions do
            #{action}
          end
          """

          Igniter.Code.Common.add_code(zipper, actions_with_action)
      end
    end)
  end
end
