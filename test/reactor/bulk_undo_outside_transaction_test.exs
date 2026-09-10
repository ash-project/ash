# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkUndoOutsideTransactionTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia, domain: Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain, [Post])
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  for step <- [
        Ash.Reactor.BulkUpdateStep,
        Ash.Reactor.BulkCreateStep,
        Ash.Reactor.BulkDestroyStep
      ] do
    test "#{inspect(step)} disallows outside_transaction undo while in a transaction" do
      step = unquote(step)
      options = [undo: :outside_transaction, resource: Post, transaction: :all]
      impl = %{impl: {step, options}}

      refute Ash.DataLayer.in_transaction?(Post)
      assert step.can?(impl, :undo)

      result =
        Ash.DataLayer.transaction(Post, fn ->
          {Ash.DataLayer.in_transaction?(Post), step.can?(impl, :undo)}
        end)

      assert {:ok, {true, false}} = result
    end
  end
end
