defmodule Ash.Resource.ManualUpdate.Function do
  @moduledoc false
  use Ash.Resource.ManualUpdate

  def update(changeset, [fun: {m, f, a}], context) do
    apply(m, f, [changeset, context | a])
  end

  def update(query, data_layer_query, [fun: fun], context) do
    fun.(query, data_layer_query, context)
  end
end
