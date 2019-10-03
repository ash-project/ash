defmodule Ash.JsonApi.Controllers.Get do
  def init(options) do
    # initialize options
    options
  end

  def call(%{path_params: %{"id" => id} = path_params} = conn, options) do
    resource = options[:resource]

    request = %Ash.Request{
      resource: resource,
      action: :get,
      path_params: path_params
    }

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
