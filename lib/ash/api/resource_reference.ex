defmodule Ash.Api.ResourceReference do
  @moduledoc "Represents a resource in an API"

  defstruct [:resource, :as]

  @type t :: %__MODULE__{}
end
