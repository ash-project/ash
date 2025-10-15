# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.TypeCheckingTest do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ValidChange do
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      changeset
    end
  end

  defmodule InvalidReturnChange do
    use Ash.Resource.Change

    def change(_changeset, _opts, _context) do
      "this should be a changeset"
    end
  end

  defmodule Resource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create_with_valid_change do
        change ValidChange
      end

      create :create_with_invalid_change do
        change InvalidReturnChange
      end
    end
  end

  test "valid change returning changeset works" do
    changeset = Ash.Changeset.for_create(Resource, :create_with_valid_change)
    context = struct(Ash.Resource.Change.Context, %{})

    result = Ash.Resource.Change.change(ValidChange, changeset, [], context)
    assert %Ash.Changeset{} = result
  end

  test "invalid change returning wrong type raises helpful error" do
    changeset = Ash.Changeset.for_create(Resource, :create_with_valid_change)
    context = struct(Ash.Resource.Change.Context, %{})

    assert_raise Ash.Error.Framework.InvalidReturnType,
                 ~r/Invalid value returned from.*change\/3/,
                 fn ->
                   Ash.Resource.Change.change(InvalidReturnChange, changeset, [], context)
                 end
  end

  test "type checking works through normal action flow" do
    # Valid change should work
    assert %Resource{} = Ash.create!(Resource, %{}, action: :create_with_valid_change)

    # Invalid change should fail during action execution
    assert_raise Ash.Error.Framework, ~r/Invalid value returned from.*change\/3/, fn ->
      Ash.create!(Resource, %{}, action: :create_with_invalid_change)
    end
  end
end
