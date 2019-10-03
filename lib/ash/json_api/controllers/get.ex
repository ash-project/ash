defmodule Ash.JsonApi.Controllers.Get do
  def init(options) do
    # initialize options
    options
  end

  def call(%{path_params: %{"id" => id}} = conn, options) do
    resource = options[:resource]

    request = Ash.Request.from(conn, resource, :get)

    case Ash.Repo.get(resource, id) do
      nil ->
        conn
        # |> put_resp_content_type("text/plain")
        |> Plug.Conn.send_resp(404, "uh oh")

      found ->
        serialized = Ash.JsonApi.Serializer.serialize_one(request, found)

        conn
        |> Plug.Conn.put_resp_content_type("application/vnd.api+json")
        |> Plug.Conn.send_resp(200, serialized)
    end
    |> Plug.Conn.halt()
  end
end
