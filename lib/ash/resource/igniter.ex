defmodule Ash.Resource.Igniter do
  @moduledoc "Codemods for working with Ash.Resource modules"

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
