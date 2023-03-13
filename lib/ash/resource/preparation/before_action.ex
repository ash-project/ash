defmodule Ash.Resource.Preparation.BeforeAction do
  @moduledoc false

  use Ash.Resource.Preparation

  @doc false
  @spec prepare(Ash.Query.t(), keyword, map) :: Ash.Query.t()
  def prepare(query, opts, _context) do
    Ash.Query.before_action(query, opts[:callback])
  end
end
