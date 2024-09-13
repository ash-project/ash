defmodule Mix.Tasks.Ash.Gen.BaseResource do
  @moduledoc """
  Generates a base resource

  ## Example

  ```bash
  mix ash.gen.base_resource MyApp.Resource
  ```
  """
  @shortdoc "Generates a base resource. This is a module that you can use instead of `Ash.Resource`, for consistency."
  use Igniter.Mix.Task

  @impl Igniter.Mix.Task
  def igniter(igniter, [base_resource | _argv]) do
    base_resource = Igniter.Code.Module.parse(base_resource)
    base_resource_file = Igniter.Project.Module.proper_location(igniter, base_resource)

    glob = Path.join([base_resource_file, "..", "**", "*.ex"])

    app_name = Igniter.Project.Application.app_name(igniter)

    # need `Igniter.glob(igniter, path, filter)` to get all existing or new files that match a path & condition
    # for each file that defines a resource that uses `Ash.Resource`, that is "further down" from this file,
    # replace what it uses with the new base resource

    igniter
    |> Igniter.create_new_file(base_resource_file, """
    defmodule #{inspect(base_resource)} do
      defmacro __using__(opts) do
        quote do
          use Ash.Resource, unquote(opts)
        end
      end
    end
    """)
    |> Igniter.Project.Config.configure(
      "config.exs",
      app_name,
      [:base_resources],
      [base_resource],
      updater: fn list ->
        Igniter.Code.List.prepend_new_to_list(
          list,
          base_resource
        )
      end
    )
    |> Igniter.update_glob(glob, fn zipper ->
      with {:ok, zipper} <- Igniter.Code.Module.move_to_module_using(zipper, Ash.Resource),
           {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :use,
               2,
               fn function_call ->
                 function_call
                 |> Igniter.Code.Function.argument_matches_predicate?(
                   0,
                   &Igniter.Code.Common.nodes_equal?(&1, Ash.Resource)
                 )
               end
             ),
           {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 0) do
        Sourceror.Zipper.replace(zipper, base_resource)
      else
        _ ->
          zipper
      end
    end)
  end
end
