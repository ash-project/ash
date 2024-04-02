defmodule Ash.Test.Actions.ValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule CustomValidation do
    use Ash.Resource.Validation

    def validate(_changeset, _, _) do
      {:error,
       Ash.Error.Changes.InvalidAttribute.exception(
         field: :status,
         message: "status is wrong dawg",
         vars: [foo: 10]
       )}
    end
  end

  defmodule Embedded do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string
    end

    validations do
      validate CustomValidation
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
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

      validate CustomValidation, where: attribute_equals(:status, "baz")
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string
      attribute :date, :date
      attribute :status, :string
      attribute :foo, :boolean
      attribute :embedded, Embedded
      attribute :embedded_list, {:array, Embedded}
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Profile
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "validations run on create" do
    assert_raise(Ash.Error.Invalid, ~r/bio, date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.new(%{bio: "foobar"})
      |> Api.create!()
    end)
  end

  test "validations only run when their when conditions validate properly" do
    Profile
    |> Ash.Changeset.new(%{foo: false, status: "bar"})
    |> Api.create!()

    Profile
    |> Ash.Changeset.new(%{foo: true, status: "foo"})
    |> Api.create!()

    assert_raise(Ash.Error.Invalid, ~r/status: must not equal foo/, fn ->
      Profile
      |> Ash.Changeset.new(%{foo: false, status: "foo"})
      |> Api.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/status: must equal foo/, fn ->
      Profile
      |> Ash.Changeset.new(%{foo: true, status: "bar"})
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

  describe "custom validations" do
    test "they can return exceptions" do
      assert {:error, _} =
               Profile
               |> Ash.Changeset.new(%{status: "baz"})
               |> Api.create()
    end

    test "they can return exceptions inside of embedded resources" do
      assert {:error, _} =
               Profile
               |> Ash.Changeset.new(%{embedded: %{name: "thing"}})
               |> Api.create()
    end

    test "they can return exceptions inside of embedded lists" do
      assert {:error, error} =
               Profile
               |> Ash.Changeset.new(%{embedded_list: [%{name: "thing"}]})
               |> Api.create()
    end

    test "a thing" do
      changeset =
        Profile
        |> Ash.Changeset.new(%{embedded_list: [%{name: "thing"}]})

      Ash.Changeset.add_invalid_errors(
        10,
        :attribute,
        changeset,
        Ash.Resource.Info.attribute(Profile, :bio),
        Ash.Error.Changes.InvalidAttribute.exception(
          field: :foo,
          vars: []
        )
      )
    end
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
