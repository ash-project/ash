defmodule Ash.Resource.Preparation.SetContext do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  def prepare(query_or_input, opts, _context) do
    context =
      case opts[:context] do
        {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [query_or_input | a])

        other ->
          {:ok, other}
      end

    case context do
      {:ok, context} ->
        set_context_on_input(query_or_input, context)

      {:error, error} ->
        add_error_to_input(query_or_input, error)

      context ->
        set_context_on_input(query_or_input, context)
    end
  end

  defp set_context_on_input(%Ash.Query{} = query, context) do
    Ash.Query.set_context(query, context)
  end

  defp set_context_on_input(%Ash.ActionInput{} = input, context) do
    Ash.ActionInput.set_context(input, context)
  end

  defp add_error_to_input(%Ash.Query{} = query, error) do
    Ash.Query.add_error(query, error)
  end

  defp add_error_to_input(%Ash.ActionInput{} = input, error) do
    Ash.ActionInput.add_error(input, error)
  end
end
