defmodule Ash.Resource.Preparation.AfterAction do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  @doc false
  @spec prepare(Ash.Query.t() | Ash.ActionInput.t(), keyword, map) ::
          Ash.Query.t() | Ash.ActionInput.t()
  def prepare(%Ash.Query{} = query, opts, context) do
    Ash.Query.after_action(query, fn query, result -> opts[:callback].(query, result, context) end)
  end

  def prepare(%Ash.ActionInput{} = input, opts, context) do
    Ash.ActionInput.after_action(input, fn input, result ->
      opts[:callback].(input, result, context)
    end)
  end
end
