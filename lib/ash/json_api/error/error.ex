defmodule Ash.JsonApi.Error do
  @callback new(Keyword.t()) :: struct
end
