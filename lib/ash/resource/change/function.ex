defmodule Ash.Resource.Change.Function do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, [{:fun, {m, f, a}}], context) do
    apply(m, f, [changeset, context | a])
  end

  def change(changeset, [{:fun, fun}], context) do
    fun.(changeset, context)
  end
end
