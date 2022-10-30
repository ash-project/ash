defmodule Ash.Resource.ManualDestroy.Function do
  @moduledoc false
  use Ash.Resource.ManualDestroy

  def destroy(changeset, [fun: {m, f, a}], context) do
    apply(m, f, [changeset, context | a])
  end

  def destroy(query, data_layer_query, [fun: fun], context) do
    fun.(query, data_layer_query, context)
  end
end
