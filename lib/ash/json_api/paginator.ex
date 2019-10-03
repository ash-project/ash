defmodule Ash.JsonApi.Paginator do
  defstruct [:limit, :offset, :total, :query]
  require Ecto.Query

  def paginate(request, query) do
    paginator = paginator(request)
    limit = paginator.limit
    offset = paginator.offset

    new_query =
      query
      |> Ecto.Query.offset(^offset)
      |> Ecto.Query.limit(^limit)

    %{paginator | query: new_query}
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
