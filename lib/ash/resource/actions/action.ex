defmodule Ash.Resource.Actions.Action do
  defstruct [:expose?, :type, :name, :path]

  def new(name, type, opts \\ []) do
    %__MODULE__{
      name: name,
      expose?: opts[:expose?] || false,
      type: type,
      path: opts[:path] || to_string(name)
    }
  end
end
