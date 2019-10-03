defmodule Ash.JsonApi.Controllers.NoRouteFound do
  def init(options) do
    # initialize options
    options
  end

  def call(conn, _options) do
    conn
    |> Plug.Conn.send_resp(404, "no route found")
    |> Plug.Conn.halt()
  end
end
