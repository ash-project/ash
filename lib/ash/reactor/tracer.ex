defmodule Ash.Reactor.Tracer do
  @moduledoc """
  Reactor middleware which threads Ash's tracing information through to new
  processes spawned by Reactor.
  """

  use Reactor.Middleware
  alias Ash.ProcessHelpers

  @doc false
  @impl true
  def get_process_context do
    tracers = Application.get_env(:ash, :tracer, [])

    ProcessHelpers.get_context_for_transfer(tracer: tracers)
  end

  @doc false
  @impl true
  def set_process_context(context) do
    ProcessHelpers.transfer_context(context)

    :ok
  end
end
