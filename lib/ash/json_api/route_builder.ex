defmodule Ash.JsonApi.RouteBuilder do
  defmacro build_resource_routes(resource) do
    quote bind_quoted: [resource: resource] do
      Ash.JsonApi.RouteBuilder.build_get_route(resource)
      Ash.JsonApi.RouteBuilder.build_index_route(resource)
    end
  end

  defmacro build_get_route(resource) do
    quote bind_quoted: [resource: resource] do
      for %{expose?: true, type: :get, path: path} = action <- Ash.actions(resource) do
        get(path, to: Ash.JsonApi.Controllers.Get, init_opts: [resource: resource])
      end
    end
  end

  defmacro build_index_route(resource) do
    quote bind_quoted: [resource: resource] do
      for %{expose?: true, type: :index, path: path} = action <- Ash.actions(resource) do
        get(path, to: Ash.JsonApi.Controllers.Index, init_opts: [resource: resource])
      end
    end
  end
end
