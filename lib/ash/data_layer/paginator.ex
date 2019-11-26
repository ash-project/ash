defmodule Ash.DataLayer.Paginator do
  defstruct [:limit, :offset, :total, :query, :results]
  # TODO: Support more pagination strategies

  @type t :: %__MODULE__{
          limit: nil | non_neg_integer(),
          offset: nil | non_neg_integer(),
          total: nil | non_neg_integer(),
          query: Ash.query(),
          results: nil | list(Ash.resource())
        }

  @spec paginate(
          Ash.resource(),
          Ash.action(),
          Ash.query(),
          params :: %{optional(String.t()) => term}
        ) ::
          {:ok, t()} | {:error, Ash.error()}
  def paginate(_resource, %{paginate?: false}, query, _params) do
    {:ok,
     %__MODULE__{
       query: query
     }}
  end

  def paginate(resource, _action, query, params) do
    with %__MODULE__{limit: limit, offset: offset} = paginator <- paginator(params),
         {:ok, query} <- Ash.Data.offset(query, offset, resource),
         {:ok, query} <- Ash.Data.limit(query, limit, resource) do
      {:ok, %{paginator | query: query}}
    else
      {:error, error} -> {:error, error}
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
    # TODO: This will raise, should be turned into returning an error.
    String.to_integer(value)
  end

  defp to_integer(value) when is_integer(value), do: value
end
