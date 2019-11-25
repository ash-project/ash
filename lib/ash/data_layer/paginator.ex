defmodule Ash.DataLayer.Paginator do
  defstruct [:limit, :offset, :total, :query, :results]
  # TODO: Support more pagination strategies

  @type t :: %__MODULE__{
          limit: nil | non_neg_integer(),
          offset: nil | non_neg_integer(),
          total: nil | non_neg_integer(),
          query: Ash.query()
        }

  @spec paginate(Ash.resource(), Ash.query(), params :: %{optional(String.t()) => term}) ::
          {:ok, t()} | {:error, Ash.error()}
  def paginate(resource, query, %{paginate?: false}) do
    {:ok,
     %__MODULE__{
       query: query
     }}
  end

  def paginate(resource, query, params) do
    with %__MODULE__{limit: limit, offset: offset} = paginator <- paginator(params),
         {:ok, query} <- Ash.Data.offset(query, offset, resource),
         {:ok, query} <- Ash.Data.limit(query, limit, resource) do
      {:ok, %{paginator | query: query}}
    else
      {:error, error} -> {:error, error}
      other -> {:error, other}
    end
  end

  defp paginator(%{page: page}) do
    # TODO: Make limit configurable
    %__MODULE__{
      offset: Map.get(page, :offset, 0) |> to_integer(),
      limit: Map.get(page, :limit, 20) |> to_integer(),
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
