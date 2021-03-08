defmodule Ash.Api.ResourceReference do
  @moduledoc "Represents a resource in an API"

  defstruct [:resource, :as, warn_on_compile_failure?: true]

  @type t :: %__MODULE__{}
end
