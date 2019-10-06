defmodule Ash.JsonApi.Controllers.Index do
  def init(options) do
    # initialize options
    options
  end

  def call(conn, options) do
    resource = options[:resource]

    request = Ash.JsonApi.Request.from(conn, resource, :index)

    paginator = Ash.JsonApi.Paginator.paginate(request, resource)

    found = Ash.Repo.all(paginator.query)

    serialized = Ash.JsonApi.Serializer.serialize_many(request, paginator, found)

    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.api+json")
    |> Plug.Conn.send_resp(200, serialized)
    |> Plug.Conn.halt()
  end
end
