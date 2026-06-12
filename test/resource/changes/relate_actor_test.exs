# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.RelateActorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      create :create_with_account do
        argument :account, :map
        change manage_relationship(:account, type: :append_and_remove)
      end
    end

    relationships do
      belongs_to :account, Ash.Test.Resource.Changes.RelateActorTest.Account do
        public?(true)
      end
    end
  end

  defmodule Account do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :text, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Resource.Changes.RelateActorTest.Author do
        public?(true)
        allow_nil? true
      end

      belongs_to :account, Ash.Test.Resource.Changes.RelateActorTest.Account do
        public?(true)
        allow_nil? true
      end
    end

    actions do
      default_accept :*

      defaults [:read]

      create :create_with_actor do
        change relate_actor(:author)
      end

      create :create_with_actor_field do
        change relate_actor(:account, field: :account)
      end

      create :create_possibly_without_actor do
        change relate_actor(:author, allow_nil?: true)
      end

      create :create_with_actor_scalar_field do
        change relate_actor(:author, field: :author_id)
      end
    end
  end

  defmodule PostRequiringActor do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :text, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Resource.Changes.RelateActorTest.Author do
        public?(true)
        allow_nil? false
      end

      belongs_to :account, Ash.Test.Resource.Changes.RelateActorTest.Account do
        public?(true)
        allow_nil? true
      end
    end

    actions do
      default_accept :*

      defaults [:read]

      create :create_with_actor do
        change relate_actor(:author)
      end

      create :create_with_actor_field do
        change relate_actor(:account, field: :account)
      end

      create :create_possibly_without_actor do
        change relate_actor(:author, allow_nil?: true)
      end
    end
  end

  defmodule User do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read, create: :*]
    end
  end

  # --- Deepest embedded: uses relate_actor --------------------------------

  defmodule UserAuthor do
    use Ash.Resource,
      domain: Domain,
      data_layer: :embedded

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        change relate_actor(:user)
      end
    end

    relationships do
      belongs_to :user, User do
        allow_nil? false
        attribute_type :uuid
      end
    end
  end

  # --- Union wrapping UserAuthor ------------------------------------------
  # include_source?: true opts in to source propagation, but the bug
  # prevents this from reaching the inner embedded type.

  defmodule AuthorUnion do
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        include_source?: true,
        storage: :map_with_tag,
        types: [
          user_author: [
            type: UserAuthor,
            tag: :__type__,
            tag_value: "user_author"
          ]
        ]
      ]
  end

  # --- Embedded resource containing the union author ----------------------

  defmodule CommentData do
    use Ash.Resource,
      domain: Domain,
      data_layer: :embedded

    actions do
      defaults [:read, create: [:text, :author]]
    end

    attributes do
      attribute :text, :string, allow_nil?: false, public?: true
      attribute :author, AuthorUnion, allow_nil?: false, public?: true
    end
  end

  # --- Top-level union wrapping CommentData -------------------------------

  defmodule EntryData do
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        include_source?: true,
        storage: :map_with_tag,
        types: [
          comment: [
            type: CommentData,
            tag: :__type__,
            tag_value: "comment"
          ]
        ]
      ]
  end

  # --- Child resource (has the embedded data attribute) -------------------

  defmodule Child do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :data, EntryData, allow_nil?: false, public?: true
    end

    actions do
      defaults [:read, create: [:data, :parent_id]]
    end

    relationships do
      belongs_to :parent,
                 Ash.Test.Resource.Changes.RelateActorEmbeddedTest.Parent do
        allow_nil? false
        public? true
      end
    end
  end

  # --- Parent resource (manages children via relationship) ----------------

  defmodule Parent do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
      end
    end

    actions do
      defaults [:read, create: [:name]]

      update :add_child do
        require_atomic? false
        argument :children, {:array, :map}
        change manage_relationship(:children, type: :create)
      end
    end

    relationships do
      has_many :children, Child do
        destination_attribute :parent_id
      end
    end
  end

  test "relate_actor change with defaults work" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()

    params = [text: "foo"]

    post_with =
      Post
      |> Ash.Changeset.for_create(:create_with_actor, params, actor: actor)
      |> Ash.create!()

    assert post_with.author_id == actor.id

    {:error, changeset} =
      Post
      |> Ash.Changeset.for_create(:create_with_actor, params, actor: nil)
      |> Ash.create()

    assert changeset.errors |> Enum.count() == 1
  end

  test "relate_actor change works with streaming bulk create" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()

    actor_id = actor.id

    assert %Ash.BulkResult{
             records: [
               %{author_id: ^actor_id}
             ],
             errors: []
           } =
             [%{text: "foo"}]
             |> Ash.bulk_create!(PostRequiringActor, :create_with_actor,
               actor: actor,
               return_errors?: true,
               return_records?: true
             )
  end

  test "relate_actor change with field" do
    account =
      Account
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    actor =
      Author
      |> Ash.Changeset.for_create(:create_with_account, %{account: account})
      |> Ash.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create_with_actor_field, %{text: "foo"}, actor: actor)
      |> Ash.create!()

    assert post.account_id == account.id
  end

  test "relate_actor change with field extracting a scalar value" do
    # When field: extracts a scalar (e.g. a UUID string) rather than a
    # related record struct, the belongs_to branch should use the scalar
    # directly as the FK value instead of calling Map.get on it.
    author =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()

    # Use a plain map as actor where :author_id is a raw UUID string
    actor = %{author_id: author.id}

    post =
      Post
      |> Ash.Changeset.for_create(:create_with_actor_scalar_field, %{text: "foo"}, actor: actor)
      |> Ash.create!()

    assert post.author_id == author.id
  end

  test "relate_actor change with field when field is nil" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()
      |> Ash.load!(:account)

    {:error, changeset} =
      Post
      |> Ash.Changeset.for_create(:create_with_actor_field, %{text: "foo"}, actor: actor)
      |> Ash.create()

    assert changeset.errors |> Enum.count() == 1
  end

  test "relate_actor change with `allow_nil?: true` allows both nil and an actor" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()

    params = [text: "foo"]

    post_with =
      Post
      |> Ash.Changeset.for_create(:create_possibly_without_actor, params, actor: actor)
      |> Ash.create!()

    assert post_with.author_id == actor.id

    post_without =
      Post
      |> Ash.Changeset.for_create(:create_possibly_without_actor, params, actor: nil)
      |> Ash.create!()

    assert is_nil(post_without.author_id)
  end

  test "relate_actor propagates through manage_relationship into nested embedded union" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    parent =
      Parent
      |> Ash.Changeset.for_create(:create, %{name: "parent"})
      |> Ash.create!()

    child_data = [
      %{
        data: %{
          __type__: "comment",
          text: "hello",
          author: %{__type__: "user_author"}
        }
      }
    ]

    updated =
      parent
      |> Ash.Changeset.for_update(:add_child, %{children: child_data}, actor: user)
      |> Ash.update!()

    updated = Ash.load!(updated, :children)

    assert [child] = updated.children
    assert child.data.value.text == "hello"
    assert child.data.value.author.type == :user_author
    assert child.data.value.author.value.user_id == user.id
  end

  test "relate_actor propagates through direct create into nested embedded union" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()

    parent =
      Parent
      |> Ash.Changeset.for_create(:create, %{name: "parent"})
      |> Ash.create!()

    child =
      Child
      |> Ash.Changeset.for_create(
        :create,
        %{
          parent_id: parent.id,
          data: %{__type__: "comment", text: "direct", author: %{__type__: "user_author"}}
        },
        actor: user
      )
      |> Ash.create!()

    assert child.data.value.text == "direct"
    assert child.data.value.author.type == :user_author
    assert child.data.value.author.value.user_id == user.id
  end
end
