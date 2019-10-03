defmodule Ash.Request do
  require Logger

  defstruct [
    :action,
    :resource,
    :route,
    :path_params,
    :query_params,
    :url,
    :json_api_prefix
  ]

  def from(conn, resource, action) do
    request = %__MODULE__{
      resource: resource,
      action: action,
      url: Plug.Conn.request_url(conn),
      path_params: conn.path_params,
      query_params: conn.query_params,
      json_api_prefix: Application.get_env(:ash, :json_api_prefix) || ""
    }

    Logger.info("Got request: #{inspect(request)}")

    request
  end
end
