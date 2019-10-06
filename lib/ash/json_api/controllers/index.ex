defmodule Ash.JsonApi.Controllers.Index do
  def init(options) do
    # initialize options
    options
  end

  def call(conn, options) do
    resource = options[:resource]

    with {:ok, request} <- Ash.JsonApi.Request.from(conn, resource, :index),
         {:ok, query} <- Ash.resource_to_query(resource),
         {:ok, paginator} <- Ash.JsonApi.Paginator.paginate(request, query),
         {:ok, found} <- Ash.get_many(paginator.query, resource) do
      serialized = Ash.JsonApi.Serializer.serialize_many(request, paginator, found)

      conn
      |> Plug.Conn.put_resp_content_type("application/vnd.api+json")
      |> Plug.Conn.send_resp(200, serialized)
      |> Plug.Conn.halt()
    else
      {:error, error} ->
        raise "whups #{inspect(error)}"
    end
  end
end
