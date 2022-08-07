defmodule Ash.Policy.Check.ActorPresent do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_) do
    "actor is present"
  end

  @impl true
  def match?(nil, _, _opts), do: false
  def match?(_, _, _opts), do: true
end
