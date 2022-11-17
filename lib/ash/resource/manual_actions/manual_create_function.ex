defmodule Ash.Resource.ManualCreate.Function do
  @moduledoc false
  use Ash.Resource.ManualCreate

  def create(changeset, [fun: {m, f, a}], context) do
    apply(m, f, [changeset, context | a])
  end

  def create(query, data_layer_query, [fun: fun], context) do
    fun.(query, data_layer_query, context)
  end
end
