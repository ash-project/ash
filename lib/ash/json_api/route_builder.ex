defmodule Ash.JsonApi.RouteBuilder do
  defmacro build_resource_routes(resource) do
    quote bind_quoted: [resource: resource] do
      Ash.JsonApi.RouteBuilder.build_get_route(resource)
      Ash.JsonApi.RouteBuilder.build_index_route(resource)
    end
  end

  defmacro build_get_route(resource) do
    quote bind_quoted: [resource: resource] do
      get_config = Ash.actions(resource)[:get]
      path = Ash.Routes.get(resource, ":id")

      case get_config do
        true ->
          get(path, to: Ash.JsonApi.Controllers.Get, init_opts: [resource: resource])

        _ ->
          # Eventually it will be a configuration, not just a boolean
          nil
      end
    end
  end

  defmacro build_index_route(resource) do
    quote bind_quoted: [resource: resource] do
      index_config = Ash.actions(resource)[:index]
      path = Ash.Routes.index(resource)

      case index_config do
        true ->
          get(path, to: Ash.JsonApi.Controllers.Index, init_opts: [resource: resource])

        _ ->
          # Eventually it will be a configuration, not just a boolean
          nil
      end
    end
  end
end
