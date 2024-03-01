defmodule Ash.Reactor.StepUtils do
  @moduledoc false

  @doc false
  @spec maybe_set_kw(Keyword.t(), atom, any) :: Keyword.t()
  def maybe_set_kw(keywords, _key, nil), do: keywords
  def maybe_set_kw(keywords, key, value), do: Keyword.put(keywords, key, value)
end
