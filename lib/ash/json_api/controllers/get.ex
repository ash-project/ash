defmodule Ash.JsonApi.Controllers.Get do
  def init(options) do
    # initialize options
    options
  end

  def call(%{path_params: %{"id" => id}} = conn, options) do
    resource = options[:resource]

    with {:ok, request} <- Ash.JsonApi.Request.from(conn, resource, :get),
         {:record, record} when not is_nil(record) <- {:record, Ash.Repo.get(resource, id)},
         {:ok, record, includes} <-
           Ash.JsonApi.Includes.Includer.get_includes(record, request) do
      IO.inspect(includes)
      serialized = Ash.JsonApi.Serializer.serialize_one(request, record, includes)

      conn
      |> Plug.Conn.put_resp_content_type("application/vnd.api+json")
      |> Plug.Conn.send_resp(200, serialized)
    else
      {:error, error} ->
        raise "whups: #{inspect(error)}"

      {:record, nil} ->
        conn
        # |> put_resp_content_type("text/plain")
        |> Plug.Conn.send_resp(404, "uh oh")
    end
    |> Plug.Conn.halt()
  end
end
