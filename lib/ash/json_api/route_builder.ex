defmodule Ash.JsonApi.RouteBuilder do
  defmacro build_resource_routes(resource) do
    quote do
      Ash.JsonApi.RouteBuilder.build_get_route(unquote(resource))
    end
  end

  defmacro build_get_route(resource) do
    quote bind_quoted: [resource: resource] do
      get_config = Ash.actions(resource)[:get]
      path = "/" <> Ash.name(resource) <> "/:id"

      case get_config do
        true ->
          get path, to: Ash.JsonApi.Controllers.Get, init_opts: [resource: resource]
        _ ->
          # Eventually it will be a configuration, not just a boolean
          nil
      end
    end
  end
end
