defmodule Ash.JsonApi.Serializer do
  alias Ash.Request

  def serialize_many(request, paginator, records) do
    data = Enum.map(records, &serialize_one_record(request, &1))
    json_api = %{version: "1.0"}
    links = many_links(request, paginator)

    Jason.encode!(%{data: data, json_api: json_api, links: links})
  end

  def serialize_one(request, record) do
    # TODO `links` and `included`
    data = serialize_one_record(request, record)
    json_api = %{version: "1.0"}
    links = one_links(request)

    Jason.encode!(%{data: data, json_api: json_api, links: links})
  end

  defp many_links(%{url: url} = request, paginator) do
    uri = URI.parse(request.url)
    query = Plug.Conn.Query.decode(uri.query || "")

    %{
      first: first_link(uri, query, paginator),
      self: url
    }
    |> add_last_link(uri, query, paginator)
    |> add_prev_link(uri, query, paginator)
    |> add_next_link(uri, query, paginator)
  end

  defp first_link(uri, query, paginator) do
    new_query =
      query
      |> Map.put("page", %{
        limit: paginator.limit,
        offset: 0
      })
      |> Plug.Conn.Query.encode()

    uri
    |> Map.put(:query, new_query)
    |> URI.to_string()
  end

  defp add_next_link(links, _uri, _query, %{offset: offset, limit: limit, total: total})
       when not is_nil(total) and offset + limit >= total,
       do: links

  defp add_next_link(links, uri, query, %{offset: offset, limit: limit}) do
    new_query =
      query
      |> Map.put("page", %{
        limit: limit + offset,
        offset: offset
      })
      |> Plug.Conn.Query.encode()

    link =
      uri
      |> Map.put(:query, new_query)
      |> URI.to_string()

    Map.put(links, :next, link)
  end

  defp add_next_link(links, uri, query, paginator) do
    new_query =
      query
      |> Map.put("page", %{
        limit: paginator.limit,
        offset: 0
      })
      |> Plug.Conn.Query.encode()

    link =
      uri
      |> Map.put(:query, new_query)
      |> URI.to_string()

    Map.put(links, :prev, link)
  end

  defp add_prev_link(links, _uri, _query, %{offset: 0}), do: links

  defp add_prev_link(links, uri, query, paginator) do
    new_query =
      query
      |> Map.put("page", %{
        limit: paginator.limit,
        offset: 0
      })
      |> Plug.Conn.Query.encode()

    link =
      uri
      |> Map.put(:query, new_query)
      |> URI.to_string()

    Map.put(links, :prev, link)
  end

  defp add_last_link(links, _uri, _query, %{total: nil}) do
    links
  end

  defp add_last_link(links, uri, query, %{total: total, limit: limit}) do
    new_query =
      query
      |> Map.put("page", %{
        limit: limit,
        offset: total - limit
      })
      |> Plug.Conn.Query.encode()

    link =
      uri
      |> Map.put(:query, new_query)
      |> URI.to_string()

    Map.put(links, "last", link)
  end

  defp one_links(request) do
    %{
      self: request.url
    }
  end

  defp serialize_one_record(%Request{resource: resource} = request, record) do
    # TODO: `relationships` `meta`
    %{
      id: record.id,
      type: Ash.type(resource),
      attributes: serialize_attributes(resource, record),
      links: %{
        self: at_host(request, Ash.Routes.get(resource, record.id))
      }
    }
  end

  defp at_host(request, route) do
    request.url
    |> URI.parse()
    |> Map.put(:query, nil)
    |> Map.put(:path, "/" <> Path.join(request.json_api_prefix, route))
    |> URI.to_string()
  end

  defp serialize_attributes(resource, record) do
    resource
    |> Ash.attributes()
    |> Keyword.delete(:id)
    |> Enum.reduce(%{}, fn {attribute, _config}, acc ->
      Map.put(acc, attribute, Map.get(record, attribute))
    end)
  end
end
