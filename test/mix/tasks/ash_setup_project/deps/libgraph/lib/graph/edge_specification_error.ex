defmodule Graph.EdgeSpecificationError do
  @moduledoc """
  This exception is raised when a Graph function expects one or more valid edge specifications,
  but receives a term which does not match one of the allowed specification patterns.
  """
  defexception [:message]

  def exception(value) do
    msg = "Expected a valid edge specification, but got: #{inspect(value)}"
    %__MODULE__{message: msg}
  end
end
