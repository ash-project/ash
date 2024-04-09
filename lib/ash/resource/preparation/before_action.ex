defmodule Ash.Resource.Preparation.BeforeAction do
  @moduledoc false

  use Ash.Resource.Preparation

  @doc false
  @spec prepare(Ash.Query.t(), keyword, map) :: Ash.Query.t()
  def prepare(query, opts, context) do
    Ash.Query.before_action(query, fn query -> opts[:callback].(query, context) end)
  end
end
