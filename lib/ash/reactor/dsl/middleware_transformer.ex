defmodule Ash.Reactor.Dsl.MiddlewareTransformer do
  @moduledoc """
  Ensures that the required middlewares are added to the Reactor.
  """

  alias Spark.Dsl.Transformer

  use Transformer

  @doc false
  @impl true
  @spec before?(module) :: boolean
  def before?(Reactor.Dsl.Transformer), do: true
  def before?(_), do: false

  @doc false
  @impl true
  def transform(dsl_state) do
    add_middleware(dsl_state, Ash.Reactor.Tracer)
  end

  defp add_middleware(dsl_state, middleware) do
    middlewares =
      dsl_state
      |> Transformer.get_entities([:reactor, :middlewares])
      |> Enum.filter(&is_struct(&1, Reactor.Dsl.Middleware))
      |> Enum.map(& &1.module)

    if middleware in middlewares do
      {:ok, dsl_state}
    else
      {:ok, middleware} =
        Reactor.Dsl
        |> Transformer.build_entity([:reactor, :middlewares], :middleware, module: middleware)

      dsl_state =
        dsl_state
        |> Transformer.add_entity([:reactor, :middlewares], middleware)

      {:ok, dsl_state}
    end
  end
end
