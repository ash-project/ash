defmodule Ash.JsonApi do
  # Honestly, at some point json api should probably be its own thing

  defmacro build_routes(scope) do
    quote do
      require Ash.JsonApi.RouteBuilder

      scope unquote(scope) do
        for resource <- Ash.resources() do
          Ash.JsonApi.RouteBuilder.build_resource_routes(resource)
        end
      end
    end
  end
end
