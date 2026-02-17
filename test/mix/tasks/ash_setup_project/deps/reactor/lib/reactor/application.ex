# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Application do
  @moduledoc false

  use Application

  @impl true
  @spec start(any, any) :: {:error, any} | {:ok, pid}
  def start(_type, _args) do
    [
      {PartitionSupervisor, child_spec: Task.Supervisor, name: Reactor.TaskSupervisor},
      Reactor.Executor.ConcurrencyTracker
    ]
    |> Supervisor.start_link(strategy: :one_for_one, name: __MODULE__.Supervisor)
  end
end
