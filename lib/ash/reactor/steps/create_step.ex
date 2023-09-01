defmodule Ash.Reactor.CreateStep do
  @moduledoc """
  The Reactor step which is used to execute create actions.
  """
  use Reactor.Step

  alias Ash.Changeset

  def run(arguments, _context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:upsert_identity, options[:upsert_identity])
      |> maybe_set_kw(:upsert?, options[:upsert?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    options[:resource]
    |> Changeset.for_create(options[:action], arguments[:input], changeset_options)
    |> options[:api].create()
  end

  defp maybe_set_kw(keywords, _key, nil), do: keywords
  defp maybe_set_kw(keywords, key, value), do: Keyword.put(keywords, key, value)
end
