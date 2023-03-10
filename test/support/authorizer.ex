defmodule Ash.Test.Authorizer do
  @moduledoc """
  A test authorizer.
  Only works for synchronous engine requests.
  """
  @behaviour Ash.Authorizer

  use Agent

  def start_link(opts) do
    Agent.start_link(
      fn ->
        %{
          strict_check_result: maybe_forbidden(opts[:strict_check]),
          check_result: maybe_forbidden(opts[:check]),
          strict_check_context: opts[:strict_check_context]
        }
      end,
      name: __MODULE__
    )
  end

  defp maybe_forbidden(:forbidden), do: {:error, Ash.Error.Forbidden.exception([])}
  defp maybe_forbidden(other), do: other

  def initial_state(_, _, _, _), do: %{}
  def strict_check_context(_), do: get(:strict_check_context, [])

  def strict_check(state, _),
    do: get(:strict_check_result, :authorized) |> continue(state) |> wrap_authorized(state)

  def check_context(_), do: []

  def check(state, _), do: get(:check_result, :authorized) |> continue(state)

  defp continue(:continue, state), do: {:continue, state}
  defp continue(other, _), do: other

  defp wrap_authorized(:authorized, state), do: {:authorized, state}
  defp wrap_authorized(other, _), do: other

  defp get(key, default) do
    Agent.get(__MODULE__, &Map.get(&1, key)) || default
  catch
    :exit, _ ->
      default
  end
end
