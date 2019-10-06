defmodule Ash.JsonApi.Paginator do
  defstruct [:limit, :offset, :total, :query]
  require Ecto.Query
  alias Ash.JsonApi.Request

  @type t :: %__MODULE__{
          limit: nil | non_neg_integer(),
          offset: nil | non_neg_integer(),
          total: nil | non_neg_integer(),
          query: Ash.query()
        }

  @spec paginate(Request.t(), Ash.query()) :: {:ok, t()} | {:error, Ash.error()}
  def paginate(%{resource: resource} = request, query) do
    with %{limit: limit, offset: offset} = paginator <- paginator(request),
         {:ok, query} <- Ash.offset(query, offset, resource),
         {:ok, query} <- Ash.limit(query, limit, resource) do
      {:ok, %{paginator | query: query}}
    end
  end

  defp paginator(%{query_params: %{"page" => page}}) do
    # TODO: Make limit configurable
    %__MODULE__{
      offset: Map.get(page, "offset", 0) |> to_integer(),
      limit: Map.get(page, "limit", 20) |> to_integer(),
      total: nil
    }
  end

  defp paginator(_) do
    # TODO: Make limit configurable
    %__MODULE__{
      offset: 0,
      limit: 20,
      total: nil
    }
  end

  defp to_integer(value) when is_bitstring(value) do
    String.to_integer(value)
  end

  defp to_integer(value) when is_integer(value), do: value
end
