defmodule Ash.Test.Actions.ValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    validations do
      validate absent([:bio, :date]), on: :create
      validate present(:bio), on: :update
      validate absent(:date), on: :destroy

      validate one_of(:status, ["foo", "bar"])

      validate attribute_equals(:status, "foo") do
        where([attribute_equals(:foo, true)])
      end

      validate attribute_does_not_equal(:status, "foo"),
        where: attribute_equals(:foo, false)
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string
      attribute :date, :date
      attribute :status, :string
      attribute :foo, :boolean
    end
  end

  test "validations run on create" do
    assert_raise(Ash.Error.Invalid, ~r/bio, date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
      |> Domain.create!()
    end)
  end

  test "validations only run when their when conditions validate properly" do
    Profile
    |> Ash.Changeset.for_create(:create, %{foo: false, status: "bar"})
    |> Domain.create!()

    Profile
    |> Ash.Changeset.for_create(:create, %{foo: true, status: "foo"})
    |> Domain.create!()

    assert_raise(Ash.Error.Invalid, ~r/status: must not equal foo/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{foo: false, status: "foo"})
      |> Domain.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/status: must equal foo/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{foo: true, status: "bar"})
      |> Domain.create!()
    end)
  end

  test "validations run on update" do
    assert_raise(Ash.Error.Invalid, ~r/bio: must be present/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{})
      |> Domain.create!()
      |> Ash.Changeset.for_update(:update, %{})
      |> Domain.update!()
    end)
  end

  test "validations run on destroy" do
    assert_raise(Ash.Error.Invalid, ~r/date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{})
      |> Domain.create!()
      |> Ash.Changeset.for_update(:update, %{bio: "foo", date: Date.utc_today()})
      |> Domain.update!()
      |> Domain.destroy!()
    end)
  end

  describe "one_of" do
    test "it succeeds if the value is in the list" do
      Profile
      |> Ash.Changeset.for_create(:create, %{status: "foo"})
      |> Domain.create!()
    end

    test "it fails if the value is not in the list" do
      assert_raise(Ash.Error.Invalid, ~r/expected one of foo, bar/, fn ->
        Profile
        |> Ash.Changeset.for_create(:create, %{status: "blart"})
        |> Domain.create!()
      end)
    end
  end
end
