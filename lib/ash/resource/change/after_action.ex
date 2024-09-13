defmodule Ash.Resource.Change.AfterAction do
  @moduledoc false
  use Ash.Resource.Change

  @doc false
  @impl true
  def change(changeset, opts, context) do
    Ash.Changeset.after_action(
      changeset,
      fn changeset, result ->
        opts[:callback].(changeset, result, context)
      end,
      prepend?: opts[:prepend?]
    )
  end

  # we had to remove this. We can't be sure that the change function won't access
  # `changeset.data`, so cannot do this automatically
  # @impl true
  # def atomic(changeset, opts, context) do
  #   {:ok, change(changeset, opts, context)}
  # end
end
