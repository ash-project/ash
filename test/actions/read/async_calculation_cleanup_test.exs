# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Read.AsyncCalculationCleanupTest do
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule Domain do
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered? true
    end
  end

  defmodule ControlledCalculation do
    use Ash.Resource.Calculation

    @impl true
    def calculate(records, options, context) do
      controller = context.source_context.controller
      send(controller, {:calculation_started, options[:kind], self()})

      receive do
        :complete ->
          case {options[:kind], context.source_context} do
            {:failure, %{success?: true}} ->
              Enum.map(records, fn _record -> "calculated" end)

            {:failure, _context} ->
              {:error,
               Ash.Error.Changes.InvalidAttribute.exception(
                 field: :secret,
                 message: "original calculation failure"
               )}

            {:blocked, %{blocked_mode: :raise}} ->
              raise "secondary calculation exception"

            {:blocked, %{blocked_mode: :exit}} ->
              exit(:secondary_calculation_exit)

            {:blocked, %{blocked_mode: :error}} ->
              {:error,
               Ash.Error.Changes.InvalidAttribute.exception(
                 field: :other,
                 message: "secondary calculation failure"
               )}

            {_kind, _context} ->
              Enum.map(records, fn _record -> "calculated" end)
          end
      end
    end
  end

  defmodule Record do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      validate_domain_inclusion?: false

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    calculations do
      calculate :failure, :string, {ControlledCalculation, kind: :failure} do
        async? true
      end

      calculate :blocked, :string, {ControlledCalculation, kind: :blocked} do
        async? true
      end

      calculate :second_blocked, :string, {ControlledCalculation, kind: :second_blocked} do
        async? true
      end
    end
  end

  test "a typed error waits for a blocked sibling before stopping the owned limiter" do
    assert_drained_read([])
  end

  test "a typed error drains multiple siblings" do
    assert_drained_read(multiple?: true)
  end

  test "a later calculation error does not replace the original typed error" do
    assert_drained_read(multiple?: true, blocked_mode: :error)
  end

  test "a wrapped sibling exception does not skip the remaining siblings" do
    assert_drained_read(multiple?: true, blocked_mode: :raise)
  end

  test "a borrowed limiter retains its available slots after the error" do
    {:ok, limiter} = Ash.Actions.Read.AsyncLimiter.start_link(4)

    try do
      assert_drained_read(multiple?: true, limiter: limiter)
      assert Agent.get(limiter, & &1) == {1, 4}
    after
      Agent.stop(limiter)
    end
  end

  test "successful asynchronous calculations still return their values" do
    assert_drained_read(success?: true, multiple?: true)
  end

  defp assert_drained_read(options) do
    record = Ash.create!(Record, %{})
    controller = self()
    blocked_kinds = if options[:multiple?], do: [:blocked, :second_blocked], else: [:blocked]

    {reader, reader_monitor} =
      spawn_monitor(fn ->
        context = %{
          controller: controller,
          success?: options[:success?],
          blocked_mode: options[:blocked_mode]
        }

        context =
          if options[:limiter],
            do: Map.put(context, :private, %{async_limiter: options[:limiter]}),
            else: context

        result =
          Record
          |> Ash.Query.filter(id == ^record.id)
          |> Ash.Query.load([:failure | blocked_kinds])
          |> Ash.Query.set_context(context)
          |> Ash.read(max_concurrency: 4)

        send(controller, {:read_result, self(), result})

        receive do
          :finish -> :ok
        end
      end)

    assert_receive {:calculation_started, :failure, failure_task}, 2_000
    failure_monitor = Process.monitor(failure_task)

    blocked_tasks =
      Enum.map(blocked_kinds, fn kind ->
        assert_receive {:calculation_started, ^kind, task}, 2_000
        {task, Process.monitor(task)}
      end)

    try do
      send(failure_task, :complete)
      assert_receive {:DOWN, ^failure_monitor, :process, ^failure_task, :normal}, 2_000

      Enum.each(blocked_tasks, fn {task, monitor} ->
        refute_receive {:read_result, ^reader, _result}, 100
        send(task, :complete)
        assert_receive {:DOWN, ^monitor, :process, ^task, :normal}, 2_000
      end)

      assert_receive {:read_result, ^reader, result}, 2_000

      if options[:success?] do
        assert {:ok, [loaded]} = result
        assert loaded.failure == "calculated"
        assert loaded.blocked == "calculated"
        assert loaded.second_blocked == "calculated"
      else
        assert {:error, %Ash.Error.Invalid{errors: errors}} = result

        assert Enum.any?(errors, fn error ->
                 match?(
                   %Ash.Error.Changes.InvalidAttribute{
                     field: :secret,
                     message: "original calculation failure"
                   },
                   error
                 )
               end)
      end

      assert Process.alive?(reader)
      send(reader, :finish)
      assert_receive {:DOWN, ^reader_monitor, :process, ^reader, :normal}, 2_000
    after
      send(failure_task, :complete)
      Enum.each(blocked_tasks, fn {task, _monitor} -> send(task, :complete) end)
      send(reader, :finish)
    end
  end
end
