defmodule Ash.Request do
  defstruct [
    :action,
    :resource,
    :path_params
  ]
end
