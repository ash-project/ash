defmodule Ash.Flow.Step.CustomFunction do
  @moduledoc false
  use Ash.Flow.Step

  def run(input, [fun: {m, f, a}], context) do
    apply(m, f, [input, context | a])
  end

  def run(input, [fun: fun], context) do
    fun.(input, context)
  end
end
