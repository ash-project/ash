defmodule Ash.Resource.Preparation.BeforeAction do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  @doc false
  @spec prepare(Ash.Query.t() | Ash.ActionInput.t(), keyword, map) ::
          Ash.Query.t() | Ash.ActionInput.t()
  def prepare(%Ash.Query{} = query, opts, context) do
    Ash.Query.before_action(query, fn query -> opts[:callback].(query, context) end)
  end

  def prepare(%Ash.ActionInput{} = input, opts, context) do
    Ash.ActionInput.before_action(input, fn input -> opts[:callback].(input, context) end)
  end
end
