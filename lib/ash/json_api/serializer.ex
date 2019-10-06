defmodule Ash.JsonApi.Serializer do
  alias Ash.JsonApi.Request

  def serialize_many(request, paginator, records, includes \\ []) do
    data = Enum.map(records, &serialize_one_record(request, &1))
    json_api = %{version: "1.0"}
    links = many_links(request, paginator)

    %{data: data, json_api: json_api, links: links}
    |> add_includes(request, includes)
    |> Jason.encode!()
  end

  def serialize_one(request, record, includes \\ [])

  def serialize_one(request, nil, _) do
    # TODO `included`
    json_api = %{version: "1.0"}
    links = one_links(request)

    Jason.encode!(%{data: nil, json_api: json_api, links: links})
  end

  def serialize_one(request, record, includes) do
    data = serialize_one_record(request, record)
    json_api = %{version: "1.0"}
    links = one_links(request)

    %{data: data, json_api: json_api, links: links}
    |> add_includes(request, includes)
    |> Jason.encode!()
  end

  defp add_includes(payload, _request, []), do: payload

  defp add_includes(payload, request, includes) do
    includes = Enum.map(includes, &serialize_one_record(request, &1))
    Map.put(payload, :includes, includes)
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

  defp serialize_one_record(request, %resource{} = record) do
    # TODO: `relationships` `meta`
    %{
      id: record.id,
      type: Ash.type(resource),
      attributes: serialize_attributes(record),
      relationships: serialize_relationships(request, record),
      links: %{
        self: at_host(request, Ash.Routes.get(resource, record.id))
      }
    }
  end

  # TODO: Decide if this is a hard guaruntee that record struct == resource module
  defp serialize_relationships(request, %resource{} = record) do
    # TODO: links.self, links.related
    resource
    |> Ash.relationships()
    |> Enum.filter(& &1.expose?)
    |> Enum.into(%{}, fn relationship ->
      value = %{
        links: %{
          self: at_host(with_path_params(request, %{"id" => record.id}), relationship.path)
        },
        data: render_linkage(record, relationship),
        meta: %{}
      }

      {relationship.name, value}
    end)
  end

  defp render_linkage(record, %{destination: destination, cardinality: :one, name: name}) do
    # TODO: There could be another case here if a bug in the system gave us a list of more than one?
    case record do
      %{__linkage__: %{^name => [id]}} ->
        %{id: id, type: Ash.type(destination)}

      _ ->
        nil
    end
  end

  defp render_linkage(record, %{destination: destination, cardinality: :many, name: name}) do
    # TODO: There could be another case here if a bug in the system gave us a list of more than one?
    case record do
      %{__linkage__: %{^name => linkage}} ->
        type = Ash.type(destination)

        Enum.map(linkage, &%{id: &1, type: type})

      _ ->
        []
    end
  end

  defp with_path_params(request, params) do
    Map.update!(request, :path_params, &Map.merge(&1, params))
  end

  defp at_host(request, route) do
    request.url
    |> URI.parse()
    |> Map.put(:query, nil)
    |> Map.put(:path, "/" <> Path.join(request.json_api_prefix, route))
    |> Map.update!(:path, fn path ->
      path
      |> Path.split()
      |> Enum.map(fn path_element ->
        if String.starts_with?(path_element, ":") do
          "replacing #{path_element}"
          Map.get(request.path_params, String.slice(path_element, 1..-1)) || ""
        else
          path_element
        end
      end)
      |> Path.join()
    end)
    |> URI.to_string()
  end

  defp serialize_attributes(%resource{} = record) do
    resource
    |> Ash.attributes()
    |> Stream.filter(& &1.expose?)
    |> Stream.reject(&(&1.name == :id))
    |> Enum.into(%{}, fn attribute ->
      {attribute.name, Map.get(record, attribute.name)}
    end)
  end
end
