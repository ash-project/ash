# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Read.AsyncLimiter do
  @moduledoc """
  A utility for limiting the number of concurrent async operations

  Because this is an optimization, we opt to run something synchronously
  if there is no async task available in the slot. The idea here is that
  the *vast* majority of things we do async will be fast enough not to
  warrant always waiting for an async slot to be free. We may add in some
  smarter heuristics later (i.e choosing to wait for a task instead of
  doing the work sync), but for now this is a good start.
  """
  use Agent

  def start_link(limit) do
    Agent.start_link(fn -> {1, limit} end)
  end

  def async_or_inline(
        %{resource: resource, context: %{private: %{async_limiter: async_limiter}}} = query,
        opts,
        last?,
        func
      )
      when not is_nil(async_limiter) and last? != true do
    if Application.get_env(:ash, :disable_async?) do
      func.()
    else
      if Ash.DataLayer.data_layer_can?(resource, :async_engine) && !in_transaction?(query) do
        claimed? =
          Agent.get_and_update(async_limiter, fn
            {limit, limit} ->
              {false, {limit, limit}}

            {count, limit} ->
              {true, {count + 1, limit}}
          end)

        if claimed? do
          try do
            Ash.ProcessHelpers.async(
              fn ->
                func.()
              end,
              opts
            )
          after
            release(async_limiter)
          end
        else
          func.()
        end
      else
        func.()
      end
    end
  end

  def async_or_inline(_, _opts, _, func) do
    func.()
  end

  def await_all(list) do
    list
    |> Enum.map(fn
      %Task{} = task ->
        Task.await(task, :infinity)

      other ->
        other
    end)
  end

  def await_at_least_one([]), do: {[], []}

  def await_at_least_one(list) do
    {non_tasks, tasks} = Enum.split_with(list, &(!match?(%Task{}, &1)))

    case tasks do
      [] ->
        {non_tasks, []}

      tasks ->
        {complete, pending} = collect_yielded(Task.yield_many(tasks, timeout: 0))

        case non_tasks ++ complete do
          [] ->
            collect_yielded(Task.yield_many(pending, limit: 1, timeout: :infinity))

          complete ->
            {complete, pending}
        end
    end
  end

  defp collect_yielded(results) do
    Enum.reduce(results, {[], []}, fn
      {_task, {:ok, {:__exception__, e, stacktrace}}}, _acc ->
        reraise e, stacktrace

      {_task, {:ok, term}}, {complete, pending} ->
        {[term | complete], pending}

      {_task, {:exit, term}}, {complete, pending} ->
        {[{:error, term} | complete], pending}

      {task, nil}, {complete, pending} ->
        {complete, [task | pending]}
    end)
  end

  defp in_transaction?(query) do
    Enum.any?(
      List.wrap(query.resource) ++ List.wrap(query.action.touches_resources),
      &Ash.DataLayer.in_transaction?(&1)
    )
  end

  defp release(async_limiter) do
    Agent.update(async_limiter, fn
      {count, limit} ->
        {count - 1, limit}
    end)
  end
end
