defmodule Ash.Reactor.StepUtils do
  @moduledoc false

  @doc false
  @spec maybe_set_kw(Keyword.t(), atom, any) :: Keyword.t()
  def maybe_set_kw(keywords, _key, nil), do: keywords
  def maybe_set_kw(keywords, key, value), do: Keyword.put(keywords, key, value)

  @doc false
  @spec store_changeset_in_metadata(atom, Ash.Resource.record(), Ash.Changeset.t()) ::
          Ash.Resource.record()
  def store_changeset_in_metadata(step_name, record, changeset),
    do: Ash.Resource.set_metadata(record, %{__reactor__: %{step_name => %{changeset: changeset}}})

  @doc false
  @spec get_changeset_from_metadata(atom, Ash.Resource.record()) :: Ash.Changeset.t()
  def get_changeset_from_metadata(step_name, record),
    do: Ash.Resource.get_metadata(record, [:__reactor__, step_name, :changeset])
end
