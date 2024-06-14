defmodule Ash.Resource.Igniter do
  def move_to_resource(zipper) do
    app_name = Igniter.Application.app_name()

    resources = [
      Ash.Resource | Application.get_env(app_name, :base_resources) || []
    ]

    case Igniter.Code.Module.move_to_module_using(zipper, resources) do
      :error ->
        {:error,
         """
         Could not find module using Ash.Resource or any base resource.

         To configure base resources, use `config :#{app_name}, base_resources: [...]`
         """}

      {:ok, zipper} ->
        {:ok, zipper}
    end
  end

  def add_attribute(igniter, resource, attribute) do
    igniter
    |> Igniter.update_elixir_file(Igniter.Code.Module.proper_location(resource), fn zipper ->
      case move_to_resource(zipper) do
        {:ok, zipper} ->
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

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  def add_action(igniter, resource, action) do
    igniter
    |> Igniter.update_elixir_file(Igniter.Code.Module.proper_location(resource), fn zipper ->
      case move_to_resource(zipper) do
        {:ok, zipper} ->
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

        {:error, error} ->
          {:error, error}
      end
    end)
  end
end
