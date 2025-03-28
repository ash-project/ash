defmodule Type.StructTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Metadata do
    defstruct [:foo, :bar]
  end

  defmodule Embedded do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string, allow_nil?: false
      attribute :title, :string, allow_nil?: false
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:*]

        argument :dummy_metadata, :struct, constraints: [instance_of: Metadata], allow_nil?: true
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :metadata, :struct do
        public? true

        constraints instance_of: Metadata,
                    fields: [
                      foo: [type: :string, allow_nil?: false],
                      bar: [type: :integer, constraints: [min: 0]]
                    ]
      end
    end
  end

  defmodule InvalidPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, update: :*]

      create :create do
        primary? true
        accept [:*]

        argument :dummy_metadata, :struct,
          constraints: [instance_of: NonExistingModule],
          allow_nil?: true
      end
    end

    attributes do
      uuid_primary_key :id
    end
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          bar: 1
        }
      })

    assert changeset.valid?
  end

  test "allow_nil? is true by default" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          bar: "2"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar", bar: 2}
           }
  end

  test "cast result has only atom keys" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          "bar" => nil,
          foo: "bar"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar", bar: nil}
           }
  end

  test "keys that can be nil don't need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar"
        }
      })

    assert changeset.valid?
  end

  test "keys that can not be nil need to be there" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{bar: 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: %{bar: 1},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "constraints of field types are checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{foo: "hello", bar: -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :bar,
               message: "must be more than or equal to %{min}",
               private_vars: nil,
               value: %{bar: -1, foo: "hello"},
               bread_crumbs: [],
               vars: [min: 0],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "extra fields are removed" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: %{
            "foo" => "bar",
            extra: "field"
          }
        }
      )

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %Metadata{foo: "bar"}
           }
  end

  test "values are casted before checked" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          metadata: %{
            "foo" => "",
            bar: "2"
          }
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: %{:bar => "2", "foo" => ""},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "invalid instance of argument is checked" do
    changeset =
      InvalidPost
      |> Ash.Changeset.for_create(:create, %{dummy_metadata: %Metadata{}})

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidArgument{
               field: :dummy_metadata,
               message: "is invalid",
               bread_crumbs: [],
               vars: [],
               path: []
             }
           ] = changeset.errors
  end

  test "it handles nil argument" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{dummy_metadata: nil})

    assert changeset.valid?
  end
end
