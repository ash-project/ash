# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule EmbeddedResource do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string, allow_nil?: true, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    validations do
      validate present(:name)
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
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

      attribute :bio, :string do
        public?(true)
      end

      attribute :date, :date do
        public?(true)
      end

      attribute :status, :string do
        public?(true)
      end

      attribute :foo, :boolean do
        public?(true)
      end

      attribute :embedded, EmbeddedResource do
        public?(true)
      end

      attribute :embedded_list, {:array, EmbeddedResource} do
        public?(true)
      end
    end
  end

  test "validations run on create" do
    assert_raise(Ash.Error.Invalid, ~r/bio, date: must be absent/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{bio: "foobar"})
      |> Ash.create!()
    end)
  end

  test "validations only run when their when conditions validate properly" do
    Profile
    |> Ash.Changeset.for_create(:create, %{foo: false, status: "bar"})
    |> Ash.create!()

    Profile
    |> Ash.Changeset.for_create(:create, %{foo: true, status: "foo"})
    |> Ash.create!()

    assert_raise(Ash.Error.Invalid, ~r/status: must not equal \"foo\"/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{foo: false, status: "foo"})
      |> Ash.create!()
    end)

    assert_raise(Ash.Error.Invalid, ~r/status: must equal \"foo\"/, fn ->
      Profile
      |> Ash.Changeset.for_create(:create, %{foo: true, status: "bar"})
      |> Ash.create!()
    end)
  end

  test "validations run on update" do
    profile =
      Profile
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    assert_raise(Ash.Error.Invalid, ~r/bio: must be present/, fn ->
      profile
      |> Ash.Changeset.for_update(:update, %{})
      |> Ash.update!()
    end)
  end

  test "validations run on destroy" do
    profile =
      Profile
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{bio: "foo", date: Date.utc_today()})
      |> Ash.update!()

    assert_raise(Ash.Error.Invalid, ~r/date: must be absent/, fn ->
      profile
      |> Ash.destroy!()
    end)
  end

  describe "validations run for embedded resources" do
    test "they can return exceptions inside of embedded resources" do
      assert_raise(Ash.Error.Invalid, ~r/name: must be present/, fn ->
        Profile
        |> Ash.Changeset.for_create(:create, %{embedded: %{name: nil}})
        |> Ash.create!()
      end)
    end

    test "they can return exceptions inside of embedded lists" do
      assert_raise(Ash.Error.Invalid, ~r/name: must be present/, fn ->
        Profile
        |> Ash.Changeset.for_create(:create, %{embedded_list: [%{name: nil}]})
        |> Ash.create!()
      end)
    end
  end

  describe "one_of" do
    test "it succeeds if the value is in the list" do
      Profile
      |> Ash.Changeset.for_create(:create, %{status: "foo"})
      |> Ash.create!()
    end

    test "it fails if the value is not in the list" do
      assert_raise(Ash.Error.Invalid, ~r/expected one of \"foo, bar\"/, fn ->
        Profile
        |> Ash.Changeset.for_create(:create, %{status: "blart"})
        |> Ash.create!()
      end)
    end
  end

  describe "data_one_of" do
    defmodule StatusResource do
      @moduledoc false
      use Ash.Resource,
        domain: Domain,
        data_layer: Ash.DataLayer.Ets

      ets do
        private? true
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, create: :*]

        update :update_non_atomic do
          require_atomic? false

          change fn changeset, _ctx ->
            # force non-atomic
            changeset
          end
        end

        update :update_atomic do
          require_atomic? true
        end
      end

      validations do
        validate data_one_of(:status, ["allowed", "valid"]), on: :update
      end

      attributes do
        uuid_primary_key :id

        attribute :status, :string do
          public?(true)
        end
      end
    end

    test "non-atomic: succeeds when original value is in the allowed list" do
      record =
        StatusResource
        |> Ash.Changeset.for_create(:create, %{status: "allowed"})
        |> Ash.create!()

      record
      |> Ash.Changeset.for_update(:update_non_atomic, %{status: "different"})
      |> Ash.update!()
    end

    test "non-atomic: fails when original value is not in the allowed list" do
      record =
        StatusResource
        |> Ash.Changeset.for_create(:create, %{status: "not_allowed"})
        |> Ash.create!()

      assert_raise(Ash.Error.Invalid, ~r/expected one of \"allowed, valid\"/, fn ->
        record
        |> Ash.Changeset.for_update(:update_non_atomic, %{status: "something"})
        |> Ash.update!()
      end)
    end

    test "atomic: succeeds when original value is in the allowed list" do
      record =
        StatusResource
        |> Ash.Changeset.for_create(:create, %{status: "valid"})
        |> Ash.create!()

      Ash.bulk_update!([record], :update_atomic, %{status: "changed"},
        return_records?: true,
        return_errors?: true
      )
    end

    test "atomic: fails when original value is not in the allowed list" do
      record =
        StatusResource
        |> Ash.Changeset.for_create(:create, %{status: "invalid"})
        |> Ash.create!()

      assert_raise(Ash.Error.Invalid, ~r/expected one of \"allowed, valid\"/, fn ->
        Ash.bulk_update!([record], :update_atomic, %{status: "updated"}, return_errors?: true)
      end)
    end
  end

  describe "attributes_present, attributes_absent" do
    defmodule Author do
      @moduledoc false
      use Ash.Resource,
        domain: Domain,
        data_layer: Ash.DataLayer.Ets

      ets do
        private? true
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, create: :*, update: :*]

        update :fill_bar do
          require_atomic? false
          argument :bar, :integer, allow_nil?: false

          validate attributes_absent(:bar)
          validate present(:bar)

          change fn cs, _ctx ->
            arg_bar = cs |> Ash.Changeset.get_argument(:bar)
            cs |> Ash.Changeset.change_attribute(:bar, arg_bar)
          end

          validate attributes_present(:bar)
          validate present(:bar)
        end
      end

      attributes do
        uuid_primary_key :id

        attribute :bar, :integer do
          public?(true)
        end
      end
    end

    test "it checks an attribute but not an argument" do
      Author
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()
      |> Ash.Changeset.for_update(:fill_bar, %{bar: 1})
      |> Ash.update!()
    end
  end

  describe "conditional present validation with where clause" do
    defmodule Person do
      @moduledoc false
      use Ash.Resource,
        domain: Domain,
        data_layer: Ash.DataLayer.Ets

      ets do
        private? true
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, create: :*, update: :*]
      end

      validations do
        validate present([:first_name, :last_name], where: [absent(:full_name)]) do
          message "must have either first_name and last_name or full_name"
        end
      end

      attributes do
        uuid_primary_key :id

        attribute :first_name, :string do
          public?(true)
        end

        attribute :last_name, :string do
          public?(true)
        end

        attribute :full_name, :string do
          public?(true)
        end
      end
    end

    test "validation does not trigger error when full_name is present" do
      assert_raise(
        Ash.Error.Invalid,
        ~r/unknown options \[:where\], valid options are: \[:at_least, :at_most, :exactly, :attributes\]/,
        fn ->
          Person
          |> Ash.Changeset.for_create(:create, %{full_name: "John Doe"})
          |> Ash.create!()
        end
      )
    end
  end
end
