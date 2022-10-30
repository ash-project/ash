defmodule Ash.Resource.Preparation.Function do
  @moduledoc false

  use Ash.Resource.Preparation

  def prepare(query, [fun: {m, f, a}], context) do
    apply(m, f, [query, context | a])
  end

  def prepare(query, [fun: fun], context) do
    fun.(query, context)
  end
end
