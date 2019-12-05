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
          Ash.api(),
          Ash.resource(),
          Ash.action(),
          Ash.query(),
          params :: %{optional(String.t()) => term}
        ) ::
          {:ok, t()} | {:error, Ash.error()}
  def paginate(_api, _resource, %{paginate?: false}, query, _params) do
    {:ok,
     %__MODULE__{
       query: query
     }}
  end

  def paginate(api, resource, _action, query, params) do
    with {:ok, %__MODULE__{limit: limit, offset: offset} = paginator} <-
           paginator(api, resource, params),
         {:ok, query} <- Ash.DataLayer.offset(query, offset, resource),
         {:ok, query} <- Ash.DataLayer.limit(query, limit, resource) do
      {:ok, %{paginator | query: query}}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp paginator(api, resource, %{page: page}) do
    # TODO: Make limit configurable
    page_size =
      page
      |> Map.get(:limit)
      |> Kernel.||(Ash.default_page_size(api, resource))
      |> Kernel.||(20)
      |> Kernel.min(Ash.max_page_size(api, resource))

    offset = Map.get(page, :offset, 0)

    with {:offset, true} <- {:offset, is_integer(offset) and offset >= 0},
         {:limit, true} <- {:limit, is_integer(page_size) and page_size >= 0} do
      {:ok,
       %__MODULE__{
         offset: Map.get(page, :offset, 0),
         limit: page_size,
         total: nil
       }}
    else
      {:offset, false} -> {:error, "invalid offset"}
      {:limit, false} -> {:error, "invalid limit"}
    end
  end

  defp paginator(api, resource, _) do
    # TODO: Make limit configurable
    {:ok,
     %__MODULE__{
       offset: 0,
       limit: Ash.default_page_size(api, resource) || 20,
       total: nil
     }}
  end
end
