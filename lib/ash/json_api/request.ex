defmodule Ash.JsonApi.Request do
  require Logger

  alias Ash.JsonApi.Includes

  defstruct [
    :action,
    :resource,
    :path_params,
    :query_params,
    :includes,
    :url,
    :json_api_prefix
  ]

  @type t() :: %__MODULE__{}

  @type error :: {:error, Ash.JsonApi.Error.InvalidInclude.t()}

  @spec from(conn :: Plug.Conn.t(), resource :: Ash.Resource.t(), action :: atom) ::
          {:ok, t()} | {:error, error}
  def from(conn, resource, action) do
    with %Includes.Parser{allowed: allowed, disallowed: []} <-
           Includes.Parser.parse_and_validate_includes(resource, conn.query_params) do
      request = %__MODULE__{
        resource: resource,
        action: action,
        includes: allowed,
        url: Plug.Conn.request_url(conn),
        path_params: conn.path_params,
        query_params: conn.query_params,
        json_api_prefix: Application.get_env(:ash, :json_api_prefix) || ""
      }

      Logger.info("Got request: #{inspect(request)}")

      {:ok, request}
    else
      %Includes.Parser{disallowed: disallowed} ->
        {:error, Ash.JsonApi.Error.InvalidInclude.new(invalid_includes: disallowed)}
    end
  end
end
