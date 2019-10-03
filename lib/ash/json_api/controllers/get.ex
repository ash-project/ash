defmodule Ash.JsonApi.Controllers.Get do
  def init(options) do
    # initialize options
    options
  end

  def call(%{path_params: %{"id" => id}} = conn, options) do
    resource = options[:resource]
    case Ash.Repo.get(resource, id) do
      nil ->
        conn
        # |> put_resp_content_type("text/plain")
        |> Plug.Conn.send_resp(404, "uh oh")

      found ->
        conn
        # |> put_resp_content_type("text/plain")
        |> Plug.Conn.send_resp(200, "#{found.id}")
    end
    |> Plug.Conn.halt()
  end
end
