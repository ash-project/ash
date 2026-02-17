# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Middleware.Telemetry do
  @moduledoc """
  A Reactor middleware that emits telemetry events.

  The following events are emitted:

  * `[:reactor, :run, :start]`
  * `[:reactor, :run, :stop]`
  * `[:reactor, :step, :run, :start]`
  * `[:reactor, :step, :run, :stop]`
  * `[:reactor, :step, :guard, :start]`
  * `[:reactor, :step, :guard, :stop]`
  * `[:reactor, :step, :process, :start]`
  * `[:reactor, :step, :process, :stop]`
  * `[:reactor, :step, :compensate, :start]`
  * `[:reactor, :step, :compensate, :stop]`
  * `[:reactor, :step, :undo, :start]`
  * `[:reactor, :step, :undo, :stop]`

  You can provide additional metadata by placing a map under the `telemetry_metadata` context key.
  """

  use Reactor.Middleware

  @doc false
  @impl true
  def init(context) do
    start = System.monotonic_time()

    metadata =
      context
      |> Map.get(:telemetry_metadata, %{})
      |> Map.merge(context.__reactor__)

    :telemetry.execute(
      [:reactor, :run, :start],
      %{system_time: System.system_time()},
      metadata
    )

    context =
      context
      |> Map.put(__MODULE__, %{
        start_time: start,
        metadata: metadata
      })

    {:ok, context}
  end

  @doc false
  @impl true
  def complete(result, %{__MODULE__ => %{start_time: start_time, metadata: metadata}}) do
    end_time = System.monotonic_time()
    duration = end_time - start_time

    metadata =
      metadata
      |> Map.merge(%{
        status: :ok,
        result: result
      })

    :telemetry.execute(
      [:reactor, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )

    {:ok, result}
  end

  @doc false
  @impl true
  def error(error_or_errors, %{__MODULE__ => %{start_time: start_time, metadata: metadata}}) do
    end_time = System.monotonic_time()
    duration = end_time - start_time

    metadata =
      metadata
      |> Map.merge(%{
        status: :error,
        errors: error_or_errors
      })

    :telemetry.execute(
      [:reactor, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )

    :ok
  end

  @doc false
  @impl true
  def halt(%{__MODULE__ => %{start_time: start_time, metadata: metadata}} = context) do
    end_time = System.monotonic_time()
    duration = end_time - start_time

    metadata =
      metadata
      |> Map.merge(%{
        status: :halt
      })

    :telemetry.execute(
      [:reactor, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )

    {:ok, context}
  end

  @doc false
  @impl true
  def event({:process_start, pid}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        pid: pid
      })

    start_time = System.monotonic_time()
    Process.put({__MODULE__, :process_start_time}, start_time)

    :telemetry.execute(
      [:reactor, :step, :process, :start],
      %{system_time: System.system_time()},
      metadata
    )
  end

  def event({:process_terminate, pid}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        pid: pid
      })

    start_time = Process.delete({__MODULE__, :process_start_time})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :process, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:run_start, arguments}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        arguments: arguments
      })

    start_time = System.monotonic_time()
    Process.put({__MODULE__, :step_start_time, step.name}, start_time)

    :telemetry.execute(
      [:reactor, :step, :run, :start],
      %{system_time: System.system_time()},
      metadata
    )
  end

  def event({:run_complete, result}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: result,
        status: :ok
      })

    start_time = Process.delete({__MODULE__, :step_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:run_error, errors}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: errors,
        status: :error
      })

    start_time = Process.delete({__MODULE__, :step_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:run_halt, value}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: value,
        status: :halt
      })

    start_time = Process.delete({__MODULE__, :step_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:run_retry, value}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: value,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :step_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event(:run_retry, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :step_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :run, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:guard_start, guard, arguments}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        guard: guard,
        arguments: arguments,
        status: :guard
      })

    start_time = System.monotonic_time()
    Process.put({__MODULE__, :guard_start_time, step.name, guard}, start_time)

    :telemetry.execute(
      [:reactor, :step, :guard, :start],
      %{system_time: System.system_time()},
      metadata
    )
  end

  def event({:guard_fail, guard, result}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        guard: guard,
        result: result,
        status: :error
      })

    start_time = Process.delete({__MODULE__, :guard_start_time, step.name, guard})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :guard, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:guard_pass, guard}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        guard: guard,
        status: :ok
      })

    start_time = Process.delete({__MODULE__, :guard_start_time, step.name, guard})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :guard, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:compensate_start, reason}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: reason,
        status: :compensate
      })

    start_time = System.monotonic_time()
    Process.put({__MODULE__, :compensate_start_time, step.name}, start_time)

    :telemetry.execute(
      [:reactor, :step, :compensate, :start],
      %{system_time: System.system_time()},
      metadata
    )
  end

  def event(:compensate_retry, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :compensate_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :compensate, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event(:compensate_complete, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :ok
      })

    start_time = Process.delete({__MODULE__, :compensate_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :compensate, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:compensate_retry, value}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: value,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :compensate_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :compensate, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:compensate_error, errors}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: errors,
        status: :error
      })

    start_time = Process.delete({__MODULE__, :compensate_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :compensate, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:compensate_continue, result}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: result,
        status: :ok
      })

    start_time = Process.delete({__MODULE__, :compensate_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :compensate, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event(:undo_start, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :undo
      })

    start_time = System.monotonic_time()
    Process.put({__MODULE__, :undo_start_time, step.name}, start_time)

    :telemetry.execute(
      [:reactor, :step, :undo, :start],
      %{system_time: System.system_time()},
      metadata
    )
  end

  def event({:undo_error, errors}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: errors,
        status: :error
      })

    start_time = Process.delete({__MODULE__, :undo_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :undo, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event({:undo_retry, errors}, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: errors,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :undo_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :undo, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event(:undo_retry, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :retry
      })

    start_time = Process.delete({__MODULE__, :undo_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :undo, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end

  def event(:undo_complete, step, %{__MODULE__ => %{metadata: metadata}}) do
    metadata =
      metadata
      |> Map.merge(%{
        step: step,
        result: nil,
        status: :ok
      })

    start_time = Process.delete({__MODULE__, :undo_start_time, step.name})
    end_time = System.monotonic_time()
    duration = end_time - start_time

    :telemetry.execute(
      [:reactor, :step, :undo, :stop],
      %{
        system_time: System.system_time(),
        duration: duration
      },
      metadata
    )
  end
end
