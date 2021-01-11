defmodule Ash.Test.Actions.ValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      read :default
      create :default
      update :default
      destroy :default
    end

    validations do
      validate absent([:bio, :date]), on: :create
      validate present(:bio), on: :update
      validate absent(:date), on: :destroy
      validate one_of(:status, ["foo", "bar"])
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string
      attribute :date, :date
      attribute :status, :string
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      resource Profile
    end
  end

  test "validations run on create" do
    assert_raise(Ash.Error.Invalid, ~r/bio, date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.new(%{bio: "foobar"})
      |> Api.create!()
    end)
  end

  test "validations run on update" do
    assert_raise(Ash.Error.Invalid, ~r/bio: must be present/, fn ->
      Profile
      |> Ash.Changeset.new(%{})
      |> Api.create!()
      |> Ash.Changeset.new(%{})
      |> Api.update!()
    end)
  end

  test "validations run on destroy" do
    assert_raise(Ash.Error.Invalid, ~r/date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.new(%{})
      |> Api.create!()
      |> Ash.Changeset.new(%{bio: "foo", date: Date.utc_today()})
      |> Api.update!()
      |> Api.destroy!()
    end)
  end

  describe "one_of" do
    test "it succeeds if the value is in the list" do
      Profile
      |> Ash.Changeset.new(%{status: "foo"})
      |> Api.create!()
    end

    test "it fails if the value is not in the list" do
      assert_raise(Ash.Error.Invalid, ~r/expected one of foo, bar/, fn ->
        Profile
        |> Ash.Changeset.new(%{status: "blart"})
        |> Api.create!()
      end)
    end
  end
end
