defmodule Ash.Resource.Change.Load do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def change(changeset, opts, context) do
    Changeset.after_action(changeset, fn changeset, result ->
      changeset.api.load(result, opts[:target],
        authorize?: context[:authorize?],
        actor: context[:actor],
        tracer: context[:tracer]
      )
    end)
  end
end
