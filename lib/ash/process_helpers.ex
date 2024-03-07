defmodule Ash.ProcessHelpers do
  @moduledoc """
  Helpers for working with processes and Ash actions.
  """

  @doc """
  Gets all of the ash context so it can be set into a new process.

  Use `transfer_context/1` in the new process to set the context.
  """
  @spec get_context_for_transfer(opts :: Keyword.t()) :: term
  def get_context_for_transfer(opts \\ []) do
    opts[:tracer]
    |> List.wrap()
    |> Map.new(fn tracer ->
      {tracer, Ash.Tracer.get_span_context(tracer)}
    end)
  end

  @spec transfer_context(term, opts :: Keyword.t()) :: :ok
  def transfer_context(tracer_context, _opts \\ []) do
    Enum.each(tracer_context || %{}, fn {tracer, tracer_context} ->
      Ash.Tracer.set_span_context(tracer, tracer_context)
    end)
  end
end
