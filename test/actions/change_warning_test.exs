# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ChangeWarningTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias Ash.Test.Domain, as: Domain

  defmodule NestedResource do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Resource do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create_with_nested_action do
        change fn changeset, _context ->
          Ash.create!(NestedResource, %{})
          changeset
        end
      end

      create :create_with_hook do
        change fn changeset, _context ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            Ash.create!(NestedResource, %{})
            changeset
          end)
        end
      end

      create :create_with_suppressed_warning do
        change set_context(%{warn_on_actions_in_change?: false})

        change fn changeset, _context ->
          Ash.create!(NestedResource, %{})
          changeset
        end
      end
    end

    attributes do
      uuid_primary_key :id
    end
  end

  test "warns when an action is called inside a change callback" do
    log =
      capture_log(fn ->
        Resource
        |> Ash.Changeset.for_create(:create_with_nested_action, %{})
      end)

    assert log =~ "An Ash action was called from inside the `change/3` callback"
  end

  test "does not warn when an action is called from a before_action hook" do
    log =
      capture_log(fn ->
        Resource
        |> Ash.Changeset.for_create(:create_with_hook, %{})
        |> Ash.create!()
      end)

    refute log =~ "An Ash action was called from inside the `change/3` callback"
  end

  test "can silence the warning through the changeset context" do
    log =
      capture_log(fn ->
        Resource
        |> Ash.Changeset.for_create(:create_with_suppressed_warning, %{})
      end)

    refute log =~ "An Ash action was called from inside the `change/3` callback"
  end
end
