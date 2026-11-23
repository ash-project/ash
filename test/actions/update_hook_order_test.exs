# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateHookOrderTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset
  alias Ash.Test.Domain, as: Domain

  defmodule AddHooks do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context), do: add_hooks(changeset, :change)

    @impl true
    def atomic(changeset, _opts, _context), do: {:ok, add_hooks(changeset, :atomic)}

    defp add_hooks(changeset, built_with) do
      changeset
      |> Changeset.after_action(fn _changeset, result ->
        send(self(), {:after_action, {:action, built_with}})
        {:ok, result}
      end)
      |> Changeset.after_transaction(fn _changeset, result ->
        send(self(), {:after_transaction, {:action, built_with}})
        result
      end)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:title]]

      update :update do
        accept [:title]
        change AddHooks
      end

      update :update_not_atomic do
        require_atomic? false
        accept [:title]
        change AddHooks
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end
  end

  # Hooks added outside the action's changes, before and after `for_update`,
  # run in the same order relative to the action's own hooks whether the update
  # is upgraded to an atomic one (which rebuilds the changeset from the action's
  # changes) or not.
  for {path, action, atomic_upgrade?, built_with} <- [
        {"atomically", :update, true, :atomic},
        {"without atomic upgrade", :update_not_atomic, false, :change}
      ] do
    describe "an update run #{path}" do
      test "runs after_action hooks in the order they were added" do
        post = Ash.create!(Post, %{title: "before"})

        post
        |> Changeset.new()
        |> Changeset.after_action(after_action(:added_before_for_update))
        |> Changeset.after_action(after_action(:prepended_before_for_update), prepend?: true)
        |> Changeset.for_update(unquote(action), %{title: "after"})
        |> Changeset.after_action(after_action(:prepended), prepend?: true)
        |> Changeset.after_action(after_action(:appended))
        |> Ash.update!(atomic_upgrade?: unquote(atomic_upgrade?))

        assert received(:after_action) == [
                 :prepended,
                 :prepended_before_for_update,
                 :added_before_for_update,
                 {:action, unquote(built_with)},
                 :appended
               ]
      end

      test "runs after_transaction hooks in the order they were added" do
        post = Ash.create!(Post, %{title: "before"})

        post
        |> Changeset.new()
        |> Changeset.after_transaction(after_transaction(:added_before_for_update))
        |> Changeset.after_transaction(after_transaction(:prepended_before_for_update),
          prepend?: true
        )
        |> Changeset.for_update(unquote(action), %{title: "after"})
        |> Changeset.after_transaction(after_transaction(:prepended), prepend?: true)
        |> Changeset.after_transaction(after_transaction(:appended))
        |> Ash.update!(atomic_upgrade?: unquote(atomic_upgrade?))

        assert received(:after_transaction) == [
                 :prepended,
                 :prepended_before_for_update,
                 :added_before_for_update,
                 {:action, unquote(built_with)},
                 :appended
               ]
      end
    end
  end

  defp after_action(name) do
    fn _changeset, result ->
      send(self(), {:after_action, name})
      {:ok, result}
    end
  end

  defp after_transaction(name) do
    fn _changeset, result ->
      send(self(), {:after_transaction, name})
      result
    end
  end

  defp received(hook, acc \\ []) do
    receive do
      {^hook, name} -> received(hook, [name | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end
end
