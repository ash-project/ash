defmodule AshKetchumWeb.Router do
  use AshKetchumWeb, :router
  require Ash.JsonApi

  forward "/json_api", AshKetchum.Resources.Router
end
