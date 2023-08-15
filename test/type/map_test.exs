defmodule Type.MapTest do
  use ExUnit.Case, async: true

  import Ash.Changeset

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :metadata, :map do
        constraints fields: [
                      foo: [type: :string, allow_nil?: false],
                      bar: [type: :integer, constraints: [min: 0]]
                    ]
      end

      attribute :otherdata, :map
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Post
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "it handles valid maps" do
    changeset =
      Post
      |> for_create(:create, %{
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
      |> for_create(:create, %{
        metadata: %{
          foo: "bar",
          bar: "2"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", bar: 2}
           }
  end

  test "cast result has only atom keys" do
    changeset =
      Post
      |> for_create(:create, %{
        metadata: %{
          "bar" => nil,
          foo: "bar"
        }
      })

    assert changeset.valid?

    assert changeset.attributes == %{
             metadata: %{foo: "bar", bar: nil}
           }
  end

  test "keys that can be nil don't need to be there" do
    changeset =
      Post
      |> for_create(:create, %{
        metadata: %{
          foo: "bar"
        }
      })

    assert changeset.valid?
  end

  test "keys that can not be nil need to be there" do
    changeset =
      Post
      |> for_create(:create, %{
        metadata: %{bar: 1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :foo,
               message: "field must be present",
               private_vars: nil,
               value: %{bar: 1},
               changeset: nil,
               query: nil,
               error_context: [],
               vars: [field: :foo, message: "field must be present", path: [:metadata]],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "constraints of field types are checked" do
    changeset =
      Post
      |> for_create(:create, %{
        metadata: %{foo: "hello", bar: -1}
      })

    refute changeset.valid?

    assert [
             %Ash.Error.Changes.InvalidAttribute{
               field: :bar,
               message: "must be more than or equal to %{min}",
               private_vars: nil,
               value: %{bar: -1, foo: "hello"},
               changeset: nil,
               query: nil,
               error_context: [],
               vars: [
                 field: :bar,
                 message: "must be more than or equal to %{min}",
                 path: [:metadata]
               ],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "extra fields are removed" do
    changeset =
      Post
      |> for_create(
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
      |> for_create(
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
               changeset: nil,
               query: nil,
               error_context: [],
               vars: [field: :foo, message: "value must not be nil", path: [:metadata]],
               path: [:metadata]
             }
           ] = changeset.errors
  end

  test "maps without constraints are left as is" do
    changeset =
      Post
      |> for_create(
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
end
