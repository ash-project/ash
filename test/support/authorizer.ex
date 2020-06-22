defmodule Ash.Test.Authorizer do
  @moduledoc """
  A test authorizer.
  Only works for synchronous engine requests.
  """
  @behaviour Ash.Authorizer

  alias Ash.Error.Forbidden

  def initial_state(_, _, _, _), do: %{}
  def strict_check_context(_), do: Process.get(:strict_check_context, [])

  def strict_check(_, _) do
    if Process.get(:authorize?, false) do
      :authorized
    else
      {:error, Forbidden.exception([])}
    end
  end

  def check_context(_), do: []

  def check(_, _) do
    if Process.get(:authorize_check?, false) do
      :authorized
    else
      {:error, :forbidden}
    end
  end
end
