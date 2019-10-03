defmodule Ash.JsonApi.Router do
  defmacro __using__(_) do
    quote do
      use Plug.Router
      require Ash.JsonApi.RouteBuilder

      plug :match
      plug Plug.Parsers, parsers: [:json],
                        pass:  ["application/json"],
                        json_decoder: Jason
      plug :dispatch

      for resource <- Ash.resources() do
        Code.ensure_compiled(resource)

        Ash.JsonApi.RouteBuilder.build_resource_routes(resource)
      end

      match _ , to: Ash.JsonApi.Controllers.NoRouteFound
    end
  end
end
