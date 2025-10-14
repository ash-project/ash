# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.OptimisticLockTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :text, :string do
        public?(true)
      end

      attribute :version, :integer, allow_nil?: false, default: 1
    end

    actions do
      default_accept :*
      defaults [:read, :create]

      update :atomic do
        change optimistic_lock(:version)
      end

      update :non_atomic do
        require_atomic? false
        change optimistic_lock(:version)
        change fn changeset, _ -> changeset end
      end
    end
  end

  test "optimistic_lock prevents concurrent writes on non atomic actions" do
    post = Ash.create!(Post, %{text: "Hello World!"})

    Ash.update!(post, %{text: "Goodbye World!"}, action: :non_atomic)

    assert_raise Ash.Error.Invalid, ~r/Attempted to update stale record/, fn ->
      Ash.update!(post, %{text: "Goodbye World!"}, action: :non_atomic)
    end
  end

  test "optimistic_lock prevents concurrent writes on atomic actions" do
    post = Ash.create!(Post, %{text: "Hello World!"})

    Ash.update!(post, %{text: "Goodbye World!"}, action: :atomic)

    assert_raise Ash.Error.Invalid, ~r/Attempted to update stale record/, fn ->
      Ash.update!(post, %{text: "Goodbye World!"}, action: :atomic)
    end
  end
end
