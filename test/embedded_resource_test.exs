# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Changeset.EmbeddedResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset

  defmodule Increasing do
    use Ash.Resource.Validation

    def init(opts), do: {:ok, opts}

    def validate(changeset, opts, _) do
      field = Keyword.get(opts, :field)

      if Changeset.changing_attribute?(changeset, field) do
        if Map.get(changeset.data, field) >=
             Changeset.get_attribute(changeset, field) do
          {:error, message: "must be increasing", field: field}
        else
          :ok
        end
      else
        :ok
      end
    end
  end

  defmodule DestroyMe do
    use Ash.Resource.Validation

    def init(opts), do: {:ok, opts}

    def validate(changeset, _opts, _) do
      if Changeset.get_attribute(changeset, :first_name) == "destroy" &&
           Changeset.get_attribute(changeset, :last_name) == "me" do
        :ok
      else
        {:error, "must be named \"destroy me\" to remove a profile"}
      end
    end
  end

  defmodule ProfileWithId do
    use Ash.Resource, data_layer: :embedded

    attributes do
      uuid_primary_key :id, writable?: true

      attribute :first_name, :string do
        public?(true)
      end

      attribute :last_name, :string do
        public?(true)
      end

      attribute :counter, :integer, default: 0, allow_nil?: false, public?: true
    end

    validations do
      validate present([:first_name, :last_name])
      validate {Increasing, field: :counter}, on: :update
      validate {DestroyMe, []}, on: :destroy
    end

    calculations do
      calculate :full_name, :string, concat([:first_name, :last_name], " ") do
        public? true
      end
    end
  end

  defmodule Profile do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :first_name, :string do
        public?(true)
      end

      attribute :last_name, :string do
        public?(true)
      end

      attribute :counter, :integer, default: 0, allow_nil?: false, public?: true
    end

    validations do
      validate present([:first_name, :last_name])
      validate {Increasing, field: :counter}, on: :update
      validate {DestroyMe, []}, on: :destroy
    end

    calculations do
      calculate :full_name, :string, concat([:first_name, :last_name], " ") do
        public? true
      end
    end
  end

  defmodule Tag do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :name, :string do
        public?(true)
      end

      attribute :score, :integer do
        public?(true)
      end

      attribute :key, :string do
        source :different_key
      end
    end

    validations do
      # You can't remove a tag unless you first set its score to 0
      validate absent(:score), on: :destroy
      validate {Increasing, field: :score}, on: :update
      validate present(:score), on: :create
    end
  end

  defmodule TagWithNoNils do
    use Ash.Resource, data_layer: :embedded, embed_nil_values?: false

    attributes do
      attribute :name, :string do
        public?(true)
      end

      attribute :score, :integer do
        public?(true)
      end
    end

    validations do
      # You can't remove a tag unless you first set its score to 0
      validate absent(:score), on: :destroy
      validate {Increasing, field: :score}, on: :update
      validate present(:score), on: :create
    end
  end

  defmodule TagWithId do
    use Ash.Resource, data_layer: :embedded

    attributes do
      uuid_primary_key :id, writable?: true

      attribute :type, :string do
        public?(true)
      end

      attribute :name, :string do
        public?(true)
      end

      attribute :score, :integer do
        public?(true)
      end
    end

    validations do
      # You can't remove a tag unless you first set its score to 0
      validate absent(:score), on: :destroy
      validate {Increasing, field: :score}, on: :update
      validate present(:score), on: :create
    end
  end

  defmodule UnionTagWithId do
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        types: [
          tag: [
            type: TagWithId,
            tag: :type,
            tag_value: :tag_with_id
          ]
        ]
      ]
  end

  defmodule Author do
    use Ash.Resource,
      domain: Ash.Test.Changeset.EmbeddedResourceTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      create :create

      update :update do
        require_atomic? false
      end
    end

    attributes do
      uuid_primary_key :id, writable?: true

      attribute :profile, Profile,
        constraints: [
          load: [:full_name]
        ],
        public?: true

      attribute :profile_with_id, ProfileWithId,
        constraints: [
          load: [:full_name]
        ],
        public?: true

      attribute :tags, {:array, Tag} do
        public?(true)
      end

      attribute :tags_max_length, {:array, Tag} do
        public?(true)
        constraints max_length: 2, min_length: 1
      end

      attribute :tags_with_id, {:array, TagWithId} do
        public?(true)
      end

      attribute :union_tags_with_id, {:array, UnionTagWithId} do
        public?(true)
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Author
    end
  end

  test "embedded resources can be created" do
    assert %{profile: %Profile{}, tags: [%Tag{name: "trainer"}, %Tag{name: "human"}]} =
             Changeset.for_create(
               Author,
               :create,
               %{
                 profile: %{
                   first_name: "ash",
                   last_name: "ketchum"
                 },
                 tags: [
                   %{name: "trainer", score: 10},
                   %{name: "human", score: 100}
                 ]
               }
             )
             |> Ash.create!()
  end

  test "embedded resources honor the `source` of an attribute" do
    assert {:ok, %{different_key: "foo"}} = Ash.Type.dump_to_native(Tag, %Tag{key: "foo"}, [])
  end

  test "embed_nil_values?: false causes nil values not to be dumped" do
    value = %TagWithNoNils{name: "foo", score: nil}
    assert {:ok, dumped} = Ash.Type.dump_to_embedded(TagWithNoNils, value, [])
    assert Map.keys(dumped) == [:name]
  end

  test "embedded resources can be constrained with min/max length" do
    assert_raise Ash.Error.Invalid, ~r/must have 2 or fewer items/, fn ->
      Changeset.for_create(
        Author,
        :create,
        %{
          profile: %{
            first_name: "ash",
            last_name: "ketchum"
          },
          tags_max_length: [
            %{name: "trainer", score: 10},
            %{name: "human", score: 100},
            %{name: "gym_leader", score: 150}
          ]
        }
      )
      |> Ash.create!()
    end

    assert_raise Ash.Error.Invalid, ~r/must have 1 or more items/, fn ->
      Changeset.for_create(
        Author,
        :create,
        %{
          profile: %{
            first_name: "ash",
            last_name: "ketchum"
          },
          tags_max_length: []
        }
      )
      |> Ash.create!()
    end
  end

  test "embedded resources support calculations" do
    assert %{profile: %Profile{full_name: "ash ketchum"}} =
             Changeset.for_create(
               Author,
               :create,
               %{
                 profile: %{
                   first_name: "ash",
                   last_name: "ketchum"
                 }
               }
             )
             |> Ash.create!()
  end

  test "embedded resources run validations on create" do
    msg =
      ~r/Invalid value provided for last_name: exactly 2 of "first_name,last_name" must be present/

    assert_raise Ash.Error.Invalid,
                 msg,
                 fn ->
                   Author
                   |> Changeset.for_create(
                     :create,
                     %{
                       profile: %{
                         first_name: "ash"
                       }
                     }
                   )
                   |> Ash.create!()
                 end
  end

  test "embedded resources run validations on update" do
    assert author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 profile: %{
                   first_name: "ash",
                   last_name: "ketchum"
                 }
               }
             )
             |> Ash.create!()

    input = %{counter: author.profile.counter - 1}

    assert_raise Ash.Error.Invalid,
                 ~r/Invalid value provided for counter: must be increasing/,
                 fn ->
                   Changeset.for_update(
                     author,
                     :update,
                     %{
                       profile: input
                     }
                   )
                   |> Ash.update!()
                 end
  end

  test "embedded resources run validations on destroy" do
    assert author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 profile: %{
                   first_name: "ash",
                   last_name: "ketchum"
                 }
               }
             )
             |> Ash.create!()

    assert_raise Ash.Error.Invalid, ~r/must be named "destroy me" to remove a profile/, fn ->
      Changeset.for_update(
        author,
        :update,
        %{profile: nil}
      )
      |> Ash.update!()
    end

    author =
      Changeset.for_update(
        author,
        :update,
        %{profile: %{first_name: "destroy", last_name: "me"}}
      )
      |> Ash.update!()

    Changeset.for_update(
      author,
      :update,
      %{profile: nil}
    )
    |> Ash.update!()
  end

  test "when a non-array embedded resource has a public primary key, changes are considered a destroy + create, not an update" do
    assert author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 profile_with_id: %{
                   first_name: "ash",
                   last_name: "ketchum"
                 }
               }
             )
             |> Ash.create!()

    assert_raise Ash.Error.Invalid, ~r/must be named "destroy me" to remove a profile/, fn ->
      Changeset.for_update(
        author,
        :update,
        %{profile_with_id: %{first_name: "foo", last_name: "bar"}}
      )
      |> Ash.update!()
    end

    author =
      Changeset.for_update(
        author,
        :update,
        %{
          profile_with_id: %{
            id: author.profile_with_id.id,
            first_name: "destroy",
            last_name: "me"
          }
        }
      )
      |> Ash.update!()

    Changeset.for_update(
      author,
      :update,
      %{profile_with_id: %{first_name: "foo", last_name: "bar"}}
    )
    |> Ash.update!()
  end

  test "a list of embeds without an id are destroyed and created each time" do
    assert author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 tags: [
                   %{name: "trainer", score: 10},
                   %{name: "human", score: 100}
                 ]
               }
             )
             |> Ash.create!()

    assert_raise Ash.Error.Invalid,
                 ~r/Invalid value provided for score: must be present/,
                 fn ->
                   Changeset.for_update(
                     author,
                     :update,
                     %{
                       tags: [
                         %{name: "pokemon"}
                       ]
                     }
                   )
                   |> Ash.update!()
                 end

    assert_raise Ash.Error.Invalid,
                 ~r/Invalid value provided for score: must be absent/,
                 fn ->
                   Changeset.for_update(
                     author,
                     :update,
                     %{
                       tags: [
                         %{name: "pokemon", score: 1}
                       ]
                     }
                   )
                   |> Ash.update!()
                 end
  end

  test "a list of embeds are updated where appropriate" do
    assert %{tags_with_id: [tag]} =
             author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 tags_with_id: [
                   %{name: "trainer", score: 10}
                 ]
               }
             )
             |> Ash.create!()

    exception =
      assert_raise Ash.Error.Invalid,
                   ~r/Invalid value provided for score: must be increasing/,
                   fn ->
                     Changeset.for_update(
                       author,
                       :update,
                       %{
                         tags_with_id: [
                           %{id: tag.id, score: 1}
                         ]
                       }
                     )
                     |> Ash.update!()
                   end

    assert Enum.at(exception.errors, 0).path == [:tags_with_id, 0]

    applied_author =
      Changeset.for_update(
        author,
        :update,
        %{
          tags_with_id: [
            %{id: tag.id, score: 100}
          ]
        }
      )
      |> Ash.update!()

    # The ID of the Tag should not change
    assert Enum.map(applied_author.tags_with_id, & &1.id) ==
             Enum.map(author.tags_with_id, & &1.id)
  end

  test "a list of union embeds are updated where appropriate" do
    assert %{union_tags_with_id: [%Ash.Union{value: tag}]} =
             author =
             Changeset.for_create(
               Author,
               :create,
               %{
                 union_tags_with_id: [
                   %{name: "trainer", score: 10, type: "tag_with_id"}
                 ]
               }
             )
             |> Ash.create!()

    applied_author =
      Changeset.for_update(
        author,
        :update,
        %{
          union_tags_with_id: [
            %{id: tag.id, score: 100, type: "tag_with_id"}
          ]
        }
      )
      |> Ash.update!()

    # The id of the Union Tag should not change
    assert Enum.map(applied_author.union_tags_with_id, & &1.value.id) ==
             Enum.map(author.union_tags_with_id, & &1.value.id)
  end

  test "embedded resource change detection works correctly when updating with identical struct" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })
      |> Ash.create!()

    original_profile = author.profile
    changeset = Changeset.for_update(author, :update, %{profile: original_profile})

    refute Changeset.changing_attribute?(changeset, :profile)
  end

  test "embedded resource change detection works correctly when updating with semantically identical map values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })
      |> Ash.create!()

    changeset_with_identical_map =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })

    refute Changeset.changing_attribute?(changeset_with_identical_map, :profile)
    refute Map.has_key?(changeset_with_identical_map.attributes, :profile)
  end

  test "changeset changing_attributes? returns false for embedded resources with semantically identical values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe", counter: 5}
      })
      |> Ash.create!()

    changeset_with_identical_values =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "John", last_name: "Doe", counter: 5}
      })

    refute Changeset.changing_attributes?(changeset_with_identical_values)
  end

  test "changeset changing_attribute? returns true for embedded resources with different values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe", counter: 5}
      })
      |> Ash.create!()

    changeset_with_different_values =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "Jane", last_name: "Smith", counter: 6}
      })

    assert Changeset.changing_attribute?(changeset_with_different_values, :profile)
  end

  test "changeset changing_attributes? returns false when using string keys with semantically identical embedded resource values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe", counter: 5}
      })
      |> Ash.create!()

    changeset_with_string_keys =
      Changeset.for_update(author, :update, %{
        profile: %{"first_name" => "John", "last_name" => "Doe", "counter" => 5}
      })

    refute Changeset.changing_attributes?(changeset_with_string_keys)
  end

  test "changed? context flag is false after executing update with semantically identical embedded resource values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })
      |> Ash.create!()

    changeset =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })

    {_result, notifications} = Ash.update!(changeset, return_notifications?: true)

    assert List.first(notifications).changeset.context[:changed?] == false
  end

  test "changed? context flag is true after executing update with different embedded resource values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })
      |> Ash.create!()

    changeset =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "Jane", last_name: "Smith"}
      })

    {_result, notifications} = Ash.update!(changeset, return_notifications?: true)

    assert List.first(notifications).changeset.context[:changed?] == true
  end

  test "changed? context flag is false after executing update with string keys but identical embedded resource values" do
    author =
      Author
      |> Changeset.for_create(:create, %{
        profile: %{first_name: "John", last_name: "Doe"}
      })
      |> Ash.create!()

    first_changeset =
      Changeset.for_update(author, :update, %{
        profile: %{first_name: "Jane", last_name: "Smith"}
      })

    updated_author = Ash.update!(first_changeset)

    string_key_changeset =
      Changeset.for_update(updated_author, :update, %{
        profile: %{"first_name" => "Jane", "last_name" => "Smith"}
      })

    {_result, notifications} = Ash.update!(string_key_changeset, return_notifications?: true)

    assert List.first(notifications).changeset.context[:changed?] == false
  end

  test "casting embedded resources without notifiers inside a transaction does not queue notifications" do
    # Simulates being inside an outer transaction, where notifications are
    # deferred into the process dictionary until the transaction completes.
    Process.put(:ash_started_transaction?, true)

    try do
      for i <- 1..25 do
        input = %{first_name: "first_#{i}", last_name: "last_#{i}"}
        {:ok, profile} = Ash.Type.cast_input(Profile, input)
        {:ok, _} = Ash.Type.apply_constraints(Profile, profile, [])

        {:ok, [profile]} = Ash.Type.cast_input({:array, Profile}, [input])
        {:ok, _} = Ash.Type.apply_constraints({:array, Profile}, [profile], [])
      end

      assert Process.get(:ash_notifications, []) == []
    after
      Process.delete(:ash_started_transaction?)
      Process.delete(:ash_notifications)
    end
  end

  describe "casting errors in arrays of simple embedded resources" do
    defmodule CastErrorType do
      use Ash.Type

      def storage_type(_), do: :string
      def cast_input(nil, _), do: {:ok, nil}
      def cast_input(value, _), do: {:error, value}
      def cast_stored(value, _), do: {:ok, value}
      def dump_to_native(value, _), do: {:ok, value}
    end

    defmodule SimpleEmbed do
      use Ash.Resource, data_layer: :embedded

      attributes do
        attribute :id, :uuid, allow_nil?: false, public?: true
        attribute :quantity, :decimal, constraints: [greater_than: 0], public?: true
        attribute :custom, CastErrorType, public?: true

        attribute :metadata, :map do
          public? true
          constraints fields: [count: [type: :integer, constraints: [min: 1]]]
        end
      end
    end

    defmodule EmbedContainer do
      use Ash.Resource, data_layer: :embedded

      actions do
        default_accept [:entries]

        create :create do
          primary? true
          argument :entry_inputs, {:array, SimpleEmbed}
        end
      end

      attributes do
        attribute :entries, {:array, SimpleEmbed}, public?: true
      end
    end

    test "scalar casting failures retain their field, message and original input" do
      for {field, value} <- [id: "not-a-uuid", quantity: "not-a-number"] do
        valid = %{id: Ash.UUID.generate(), quantity: "1"}
        invalid = Map.put(valid, field, value)

        assert {:error, error} = Ash.Type.cast_input({:array, SimpleEmbed}, [valid, invalid])

        assert [
                 %Ash.Error.Changes.InvalidAttribute{
                   field: ^field,
                   message: "is invalid",
                   value: ^value,
                   path: [1]
                 }
               ] = Ash.Error.to_error_class(error).errors
      end
    end

    test "constraint failures retain the message template and interpolation variables" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}

      assert {:error, error} =
               Ash.Type.cast_input({:array, SimpleEmbed}, [valid, %{valid | quantity: "0"}])

      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 field: :quantity,
                 message: "must be greater than %{greater_than}",
                 value: "0",
                 vars: vars,
                 path: [1]
               }
             ] = Ash.Error.to_error_class(error).errors

      assert Decimal.equal?(vars[:greater_than], 0)
    end

    test "parent attributes and arguments prepend their names without moving the leaf field" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}

      for field <- [:entries, :entry_inputs] do
        changeset =
          Ash.Changeset.for_create(EmbedContainer, :create, %{
            field => [valid, %{valid | quantity: "0"}]
          })

        assert [
                 %Ash.Error.Changes.InvalidAttribute{
                   field: :quantity,
                   path: [^field, 1],
                   message: "must be greater than %{greater_than}",
                   vars: vars
                 }
               ] = changeset.errors

        assert Decimal.equal?(vars[:greater_than], 0)
      end
    end

    test "array error details match ordinary changeset validation, including map fields" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}

      for {field, value} <- [
            id: "not-a-uuid",
            quantity: "not-a-number",
            quantity: "0",
            metadata: %{count: 0},
            metadata: %{count: "not-an-integer"},
            custom: [message: "invalid child", field: :child, path: [:nested]],
            custom: [message: "invalid value", path: [:nested]]
          ] do
        invalid = Map.put(valid, field, value)
        changeset = Ash.Changeset.for_create(SimpleEmbed, :create, invalid)
        refute changeset.valid?
        assert {:error, error} = Ash.Type.cast_input({:array, SimpleEmbed}, [valid, invalid])

        expected =
          Enum.map(changeset.errors, fn error ->
            error
            |> Ash.Error.set_path(1)
            |> Map.take([:__struct__, :field, :path, :message, :value, :vars])
          end)

        actual =
          error
          |> Ash.Error.to_error_class()
          |> Map.fetch!(:errors)
          |> Enum.map(&Map.take(&1, [:__struct__, :field, :path, :message, :value, :vars]))

        assert actual == expected
      end
    end

    test "row indexes include already-cast structs" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}
      assert {:ok, casted} = Ash.Type.cast_input(SimpleEmbed, valid)

      assert {:error, error} =
               Ash.Type.cast_input({:array, SimpleEmbed}, [
                 casted,
                 valid,
                 %{valid | quantity: "0"}
               ])

      assert [%Ash.Error.Changes.InvalidAttribute{field: :quantity, path: [2]}] =
               Ash.Error.to_error_class(error).errors
    end

    test "required errors retain their field and row path" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}
      assert {:error, error} = Ash.Type.cast_input({:array, SimpleEmbed}, [valid, %{}])

      assert [%Ash.Error.Changes.Required{field: :id, path: [1]}] =
               Ash.Error.to_error_class(error).errors
    end

    test "multiple raw errors keep their fields, relative paths and interpolation variables" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}

      errors = [
        "invalid format",
        [message: "must be at least %{minimum}", minimum: 2],
        [message: "invalid child", fields: [:first, :second], path: [:nested]]
      ]

      assert {:error, error} =
               Ash.Type.cast_input({:array, SimpleEmbed}, [valid, Map.put(valid, :custom, errors)])

      assert [
               %Ash.Error.Changes.InvalidAttribute{
                 field: :custom,
                 message: "invalid format",
                 path: [1],
                 value: ^errors
               },
               %Ash.Error.Changes.InvalidAttribute{
                 field: :custom,
                 message: "must be at least %{minimum}",
                 path: [1],
                 vars: vars
               },
               %Ash.Error.Changes.InvalidAttribute{field: :first, path: [1, :custom, :nested]},
               %Ash.Error.Changes.InvalidAttribute{field: :second, path: [1, :custom, :nested]}
             ] = Ash.Error.to_error_class(error).errors

      assert vars[:minimum] == 2
    end

    test "existing exceptions keep their type, field, value and nested path" do
      valid = %{id: Ash.UUID.generate(), quantity: "1"}

      native =
        Ash.Error.Changes.InvalidArgument.exception(
          field: :quantity,
          message: "must be at least %{minimum}",
          value: "bad",
          vars: [minimum: 2]
        )
        |> Ash.Error.set_path([:nested])

      for error <- [native, Ash.Error.to_error_class([native])] do
        assert {:error, error} =
                 Ash.Type.cast_input({:array, SimpleEmbed}, [
                   valid,
                   Map.put(valid, :custom, error)
                 ])

        assert [
                 %Ash.Error.Changes.InvalidArgument{
                   field: :quantity,
                   message: "must be at least %{minimum}",
                   value: "bad",
                   vars: [minimum: 2],
                   path: [1, :custom, :nested]
                 }
               ] = Ash.Error.to_error_class(error).errors
      end
    end
  end

  describe "error messages include field context" do
    defmodule FailingType do
      @moduledoc false
      use Ash.Type

      def storage_type(_), do: :string

      def cast_input(value, _) when is_binary(value), do: {:ok, value}
      def cast_input(_, _), do: :error

      def cast_stored("fail_bare", _), do: :error
      def cast_stored("fail_message", _), do: {:error, "custom error message"}
      def cast_stored("fail_keyword", _), do: {:error, message: "keyword error", custom: :data}
      def cast_stored(value, _) when is_binary(value), do: {:ok, value}
      def cast_stored(nil, _), do: {:ok, nil}
      def cast_stored(_, _), do: :error

      def dump_to_native("fail_bare", _), do: :error
      def dump_to_native("fail_message", _), do: {:error, "dump error message"}

      def dump_to_native("fail_keyword", _),
        do: {:error, message: "dump keyword error", custom: :data}

      def dump_to_native(nil, _), do: {:ok, nil}
      def dump_to_native(value, _) when is_binary(value), do: {:ok, value}
      def dump_to_native(_, _), do: :error
    end

    defmodule EmbeddedWithFailingType do
      @moduledoc false
      use Ash.Resource, data_layer: :embedded

      attributes do
        attribute :name, :string, public?: true
        attribute :failing_field, FailingType, public?: true
      end
    end

    test "cast_stored error includes field name for bare :error" do
      stored = %{"name" => "test", "failing_field" => "fail_bare"}

      assert {:error, error} = Ash.Type.cast_stored(EmbeddedWithFailingType, stored, [])
      assert error[:field] == :failing_field
    end

    test "cast_stored error includes field name for string error message" do
      stored = %{"name" => "test", "failing_field" => "fail_message"}

      assert {:error, [error]} = Ash.Type.cast_stored(EmbeddedWithFailingType, stored, [])
      assert error[:field] == :failing_field
      assert error[:message] == "custom error message"
    end

    test "cast_stored error includes field name for keyword error" do
      stored = %{"name" => "test", "failing_field" => "fail_keyword"}

      assert {:error, [error]} = Ash.Type.cast_stored(EmbeddedWithFailingType, stored, [])
      assert error[:field] == :failing_field
      assert error[:message] == "keyword error"
      assert error[:custom] == :data
    end

    test "dump_to_native error includes field name for bare :error" do
      value = %EmbeddedWithFailingType{name: "test", failing_field: "fail_bare"}

      assert {:error, error} = Ash.Type.dump_to_native(EmbeddedWithFailingType, value, [])
      assert error[:field] == :failing_field
    end

    test "dump_to_native error includes field name for string error message" do
      value = %EmbeddedWithFailingType{name: "test", failing_field: "fail_message"}

      assert {:error, [error]} = Ash.Type.dump_to_native(EmbeddedWithFailingType, value, [])
      assert error[:field] == :failing_field
      assert error[:message] == "dump error message"
    end

    test "dump_to_native error includes field name for keyword error" do
      value = %EmbeddedWithFailingType{name: "test", failing_field: "fail_keyword"}

      assert {:error, [error]} = Ash.Type.dump_to_native(EmbeddedWithFailingType, value, [])
      assert error[:field] == :failing_field
      assert error[:message] == "dump keyword error"
      assert error[:custom] == :data
    end

    test "array dump_to_native error includes index for bare :error" do
      values = [
        %EmbeddedWithFailingType{name: "ok", failing_field: "valid"},
        %EmbeddedWithFailingType{name: "fail", failing_field: "fail_bare"}
      ]

      assert {:error, [error]} =
               Ash.Type.dump_to_native({:array, EmbeddedWithFailingType}, values, [])

      assert error[:index] == 1
      assert error[:field] == :failing_field
    end

    test "array dump_to_native error includes index for string error message" do
      values = [
        %EmbeddedWithFailingType{name: "ok", failing_field: "valid"},
        %EmbeddedWithFailingType{name: "fail", failing_field: "fail_message"}
      ]

      assert {:error, [error]} =
               Ash.Type.dump_to_native({:array, EmbeddedWithFailingType}, values, [])

      assert error[:index] == 1
      assert error[:message] == "dump error message"
    end

    test "array dump_to_native error includes index for keyword error" do
      values = [
        %EmbeddedWithFailingType{name: "ok", failing_field: "valid"},
        %EmbeddedWithFailingType{name: "fail", failing_field: "fail_keyword"}
      ]

      assert {:error, [error]} =
               Ash.Type.dump_to_native({:array, EmbeddedWithFailingType}, values, [])

      assert error[:index] == 1
      assert error[:message] == "dump keyword error"
      assert error[:custom] == :data
    end

    test "array dump_to_native error index is correct for first element" do
      values = [
        %EmbeddedWithFailingType{name: "fail", failing_field: "fail_bare"},
        %EmbeddedWithFailingType{name: "ok", failing_field: "valid"}
      ]

      assert {:error, [error]} =
               Ash.Type.dump_to_native({:array, EmbeddedWithFailingType}, values, [])

      assert error[:index] == 0
      assert error[:field] == :failing_field
    end
  end

  describe "end-to-end: corrupted ETS data produces safe errors" do
    defmodule CorruptibleEmbed do
      @moduledoc false
      use Ash.Resource, data_layer: :embedded

      attributes do
        attribute :name, :string, public?: true
        attribute :score, :integer, public?: true
      end
    end

    defmodule ResourceWithEmbed do
      @moduledoc false

      use Ash.Resource,
        domain: Ash.Test.Changeset.EmbeddedResourceTest.EtsDomain,
        data_layer: Ash.DataLayer.Ets

      ets do
        private?(true)
      end

      actions do
        default_accept :*
        defaults [:create, :read]
      end

      attributes do
        uuid_primary_key :id
        attribute :profile, CorruptibleEmbed, public?: true
      end
    end

    defmodule EtsDomain do
      @moduledoc false
      use Ash.Domain

      resources do
        resource ResourceWithEmbed
      end
    end

    defp create_and_corrupt_record(corrupted_profile) do
      record =
        ResourceWithEmbed
        |> Ash.Changeset.for_create(:create, %{profile: %{name: "valid", score: 42}})
        |> Ash.create!()

      table = Process.get({:ash_ets_table, ResourceWithEmbed, nil})
      pkey = %{id: record.id}
      {key, stored} = ETS.Set.get!(table, pkey)

      corrupted = %{stored | profile: corrupted_profile}
      ETS.Set.put(table, {key, corrupted})

      record
    end

    test "reading a record with corrupted embedded data returns an Ash.Error.Invalid" do
      create_and_corrupt_record(%{name: "valid", score: "not_an_integer"})

      assert {:error, %Ash.Error.Invalid{}} = Ash.read(ResourceWithEmbed)
    end

    test "error from corrupted embedded data does not expose raw values" do
      create_and_corrupt_record(%{name: "valid", score: "not_an_integer"})

      {:error, error} = Ash.read(ResourceWithEmbed)

      # The error message should not leak the raw corrupted value to end users
      error_string = Exception.message(error)
      refute error_string =~ "not_an_integer"
    end
  end
end
