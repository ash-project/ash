defmodule Ash.Test.Helpers do
  @moduledoc false

  defmacro defposts(do: body) do
    quote do
      {:module, mod, _, _} =
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false
          use Ash.Resource, api: Ash.Test.AnyApi, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          unquote(body)
        end

      mod
    end
  end

  defmacro hydrated_expr(resource, expr) do
    quote do
      Ash.Query.expr(unquote(expr))
      |> Ash.Filter.hydrate_refs(%{
        resource: unquote(resource),
        aggregates: %{},
        calculations: %{},
        public?: false
      })
    end
  end
end
