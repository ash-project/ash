defmodule Ash.Api.ResourceReference do
  @moduledoc "Represents a resource in an API"
  defstruct [:resource, warn_on_compile_failure?: true]
end
