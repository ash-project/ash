defmodule Ash.Resource.Preparation.SetContext do
  @moduledoc false

  use Ash.Resource.Preparation

  def prepare(query, opts, _context) do
    context =
      case opts[:context] do
        {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [query | a])

        other ->
          {:ok, other}
      end

    case context do
      {:ok, context} ->
        Ash.Query.set_context(query, context)

      {:error, error} ->
        Ash.Query.add_error(query, error)
    end
  end
end
