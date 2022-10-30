defmodule Ash.Resource.ManualRead.Function do
  @moduledoc false
  use Ash.Resource.ManualRead

  def read(query, data_layer_query, [fun: {m, f, a}], context) do
    apply(m, f, [query, data_layer_query, context | a])
  end

  def read(query, data_layer_query, [fun: fun], context) do
    fun.(query, data_layer_query, context)
  end
end
