defmodule AshKetchumWeb.PageController do
  use AshKetchumWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
