defmodule Ash.Type.MapTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :metadata, :map do
        public? true

        constraints fields: [
                      foo: [type: :string, allow_nil?: false],
                      integer_min_0: [type: :integer, constraints: [min: 0]],
                      string_min_3: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [min_length: 3]
                      ],
                      string_max_3: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [max_length: 3]
                      ],
                      string_match: [
                        type: :string,
                        allow_nil?: true,
                        constraints: [match: "a_A"]
                      ]
                    ]
      end

      attribute :otherdata, :map do
        public? true
      end
    end
  end

  defmodule MapWithFields do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          foo: [type: :string, allow_nil?: false]
        ]
      ]
  end

  test "cast_stored honors string keys" do
    {:ok, constraints} = Ash.Type.init(MapWithFields, [])

    assert {:ok, %{foo: "bar"}} =
             Ash.Type.cast_stored(MapWithFields, %{"foo" => "bar"}, constraints)
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          integer_min_0: 1
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
          integer_min_0: "2"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", integer_min_0: 2}
           }
  end

  test "cast result has only atom keys" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          "integer_min_0" => nil,
          foo: "bar"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", integer_min_0: nil}
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
        metadata: %{integer_min_0: 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: %{integer_min_0: 1},
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
        metadata: %{foo: "hello", integer_min_0: -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :integer_min_0,
               message: "must be more than or equal to %{min}",
               private_vars: nil,
               value: %{integer_min_0: -1, foo: "hello"},
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
             metadata: %{foo: "bar"}
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
            integer_min_0: "2"
          }
        }
      )

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "value must not be nil",
               private_vars: nil,
               value: %{:integer_min_0 => "2", "foo" => ""},
               bread_crumbs: [],
               vars: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "maps without constraints are left as is" do
    changeset =
      Post
      |> Ash.Changeset.for_create(
        :create,
        %{
          otherdata: %{
            extra: "field"
          }
        }
      )

    assert changeset.valid?

    assert changeset.attributes == %{
             otherdata: %{extra: "field"}
           }
  end

  test "multiple values with constraint errors" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_min_3: "a",
          integer_min_0: -1
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "must be more than or equal to %{min}",
               vars: [min: 0],
               field: :integer_min_0,
               private_vars: nil,
               value: %{string_min_3: "a", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "string_min_3 validates length" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_min_3: "a"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "length must be greater than or equal to %{min}",
               vars: [min: 3],
               field: :string_min_3,
               private_vars: nil,
               value: %{string_min_3: "a", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "string_max_3 validates length" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_max_3: "aaaa"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "length must be less than or equal to %{max}",
               vars: [max: 3],
               field: :string_max_3,
               private_vars: nil,
               value: %{string_max_3: "aaaa", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "string_match validates against regex pattern" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{
        metadata: %{
          foo: "bar",
          string_match: "invalid"
        }
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               message: "must match the pattern %{regex}",
               vars: [regex: "~r/a_A/"],
               field: :string_match,
               private_vars: nil,
               value: %{string_match: "invalid", foo: "bar"},
               bread_crumbs: [],
               path: [:metadata]
             }
           ] = changeset.errors
  end
end
