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

  @doc """
  Creates a task that will properly transfer the ash context to the new process
  """
  def async(func, opts) do
    ash_context = Ash.ProcessHelpers.get_context_for_transfer(opts)
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    Task.async(fn ->
      try do
        Ash.ProcessHelpers.transfer_context(ash_context, opts)

        func.()
      rescue
        e ->
          e =
            if Ash.Error.ash_error?(e) do
              case e do
                %{stacktrace: %{stacktrace: stacktrace}} when not is_nil(stacktrace) ->
                  update_in(e.stacktrace.stacktrace, &(&1 ++ Enum.drop(stacktrace, 1)))

                _ ->
                  e
              end
            else
              e
            end

          {:__exception__, e, __STACKTRACE__ ++ Enum.drop(stacktrace, 1)}
      end
    end)
  end

  @doc """
  Creates a task that will properly transfer the ash context to the new process, and timeout if it takes longer than the given timeout
  """
  def task_with_timeout(fun, resource, timeout, name, tracer) do
    if Application.get_env(:ash, :disable_async?) do
      fun.()
    else
      if (is_nil(resource) ||
            Ash.DataLayer.data_layer_can?(resource, :async_engine)) && timeout &&
           timeout != :infinity &&
           !Ash.DataLayer.in_transaction?(resource) do
        task =
          async(
            fun,
            tracer: tracer
          )

        try do
          case Task.await(task, timeout) do
            {:__exception__, e, stacktrace} ->
              reraise e, stacktrace

            other ->
              other
          end
        catch
          :exit, {:timeout, {Task, :await, [^task, timeout]}} ->
            {:error, Ash.Error.Invalid.Timeout.exception(timeout: timeout, name: name)}
        end
      else
        fun.()
      end
    end
  end
end
