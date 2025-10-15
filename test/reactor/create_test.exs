# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorCreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :organisation
      global? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false, public?: true
      attribute :organisation, :string, allow_nil?: true, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      destroy :undo_create do
        argument :changeset, :struct do
          constraints instance_of: Ash.Changeset
        end
      end
    end

    relationships do
      has_many :posts, Ash.Test.ReactorCreateTest.Post do
        public? true
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :sub_title, :string, public?: true
      attribute :deleted, :boolean, default: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :with_actor_as_author do
        change relate_actor(:author)
      end

      update :soft_delete do
        accept []

        argument :changeset, :struct do
          constraints instance_of: Ash.Changeset
        end

        change set_attribute(:deleted, true)
      end
    end

    relationships do
      belongs_to :author, Ash.Test.ReactorCreateTest.Author do
        public? true
        allow_nil? true
      end
    end
  end

  test "it can create a post" do
    defmodule SimpleCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :title
      input :sub_title

      create :create_post, Post, :create do
        inputs(%{title: input(:title), sub_title: input(:sub_title)})
      end
    end

    assert {:ok, post} =
             Reactor.run(SimpleCreatePostReactor, %{title: "Title", sub_title: "Sub-title"})

    assert post.title == "Title"
    assert post.sub_title == "Sub-title"
    assert post.__meta__.state == :loaded
  end

  test "it defaults to the primary action when the action is not supplied" do
    defmodule InferredActionNameCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :title
      input :sub_title

      create :create_post, Post do
        inputs(%{title: input(:title), sub_title: input(:sub_title)})
      end
    end

    assert {:ok, _post} =
             Reactor.run(InferredActionNameCreatePostReactor, %{
               title: "Title",
               sub_title: "Sub-title"
             })
  end

  test "it merges multiple `inputs` entities together" do
    defmodule MergedInputsCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :title
      input :sub_title

      create :create_post, Post, :create do
        inputs(%{title: input(:title)})
        inputs(%{sub_title: input(:sub_title)})
      end
    end

    assert {:ok, post} =
             Reactor.run(MergedInputsCreatePostReactor, %{
               title: "Title",
               sub_title: "Sub-title"
             })

    assert post.title == "Title"
    assert post.sub_title == "Sub-title"
  end

  test "`inputs` entities can be transformed separately" do
    defmodule TransformedInputsCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :title

      create :create_post, Post, :create do
        inputs %{title: input(:title)} do
          transform &%{title: String.upcase(&1.title)}
        end

        inputs %{sub_title: input(:title)} do
          transform &%{sub_title: String.downcase(&1.sub_title)}
        end
      end
    end

    assert {:ok, post} = Reactor.run(TransformedInputsCreatePostReactor, %{title: "Title"})

    assert post.title == "TITLE"
    assert post.sub_title == "title"
  end

  test "it can provide an actor" do
    defmodule CreateWithActorCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :author_name
      input :title
      input :sub_title

      create :create_author, Author, :create do
        inputs(%{name: input(:author_name)})
      end

      create :create_post, Post, :with_actor_as_author do
        inputs(%{title: input(:title), sub_title: input(:sub_title)})
        actor(result(:create_author))
      end
    end

    assert {:ok, post} =
             Reactor.run(CreateWithActorCreatePostReactor, %{
               author_name: "Marty McFly",
               title: "Title",
               sub_title: "Sub-title"
             })

    assert post.author.name == "Marty McFly"
    assert post.title == "Title"
  end

  test "it can provide a tenant" do
    defmodule TenantedCreateAuthorReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :author_name
      input :organisation_name

      create :create_author, Author, :create do
        inputs(%{name: input(:author_name)})
        tenant(input(:organisation_name))
      end
    end

    assert {:ok, author} =
             Reactor.run(TenantedCreateAuthorReactor, %{
               author_name: "Marty McFly",
               organisation_name: "Hill Valley High School"
             })

    assert author.name == "Marty McFly"
    assert author.organisation == "Hill Valley High School"
  end

  test "it can undo the creation on error" do
    defmodule UndoingCreateAuthorReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_domain(Domain)
      end

      input :author_name

      create :create_author, Author, :create do
        inputs(%{name: input(:author_name)})

        undo :always
        undo_action(:undo_create)
      end

      step :fail do
        argument :author, result(:create_author)

        run fn _, _ ->
          assert [_] = Ash.read!(Author)

          raise "hell"
        end
      end
    end

    UndoingCreateAuthorReactor
    |> Reactor.run(%{author_name: "Marty McFly"}, %{}, async?: false)
    |> Ash.Test.assert_has_error(fn
      %Reactor.Error.Invalid.RunStepError{error: %RuntimeError{message: "hell"}} ->
        true

      _ ->
        false
    end)

    assert [] = Ash.read!(Author)
  end

  test "it can undo the creation with an update action on error" do
    defmodule UndoingCreatePostWithUpdateReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_domain(Domain)
      end

      input :title
      input :sub_title

      create :create_post, Post, :create do
        inputs(%{title: input(:title), sub_title: input(:sub_title)})

        undo :always
        undo_action(:soft_delete)
      end

      step :fail do
        argument :post, result(:create_post)

        run fn _, _ ->
          assert [post] = Ash.read!(Post)
          assert post.deleted == false

          raise "hell"
        end
      end
    end

    UndoingCreatePostWithUpdateReactor
    |> Reactor.run(%{title: "Test Post", sub_title: "Test Sub"}, %{}, async?: false)
    |> Ash.Test.assert_has_error(fn
      %Reactor.Error.Invalid.RunStepError{error: %RuntimeError{message: "hell"}} ->
        true

      _ ->
        false
    end)

    # The post should still exist but be marked as deleted
    assert [post] = Ash.read!(Post)
    assert post.deleted == true
    assert post.title == "Test Post"
  end

  test "it can be provided a changeset as the initial value" do
    defmodule CreateFromChangesetReactor do
      @moduledoc false
      use Ash.Reactor

      step :create_changeset do
        run fn _ ->
          changeset =
            Post
            |> Ash.Changeset.for_create(:create, %{title: "Foo", sub_title: "Bar"})

          {:ok, changeset}
        end
      end

      create :create_post, Post do
        initial(result(:create_changeset))
      end

      return :create_post
    end

    assert {:ok, post} = Reactor.run(CreateFromChangesetReactor, %{}, %{}, async?: false)

    assert post.title == "Foo"
    assert post.sub_title == "Bar"
  end
end
