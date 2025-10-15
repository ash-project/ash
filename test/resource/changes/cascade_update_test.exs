# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Change.CascadeUpdate do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain
  alias Ash.Test.Resource.Change.CascadeUpdate, as: Test

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      if notification.action.name == :update do
        Agent.update(
          Test.Agent,
          &%{&1 | notifications: MapSet.put(&1.notifications, notification.data.id)}
        )
      end

      :ok
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      update :update do
        primary? true
        require_atomic? false
        skip_unknown_inputs [:content]

        change cascade_update(:posts,
                 copy_inputs: [:name, :content],
                 return_notifications?: true
               )
      end

      update :update_with_atomic_upgrade do
        require_atomic? false
        skip_unknown_inputs [:content]

        change cascade_update(:posts,
                 action: :update_with_atomic_upgrade,
                 copy_inputs: [:name, :content],
                 return_notifications?: true
               )
      end

      update :update_atomic do
        change cascade_update(:posts,
                 copy_inputs: [:name]
               )
      end

      update :wrong_relationship_cascade do
        change cascade_update(:postssss,
                 copy_inputs: [:name]
               )
      end

      update :no_notification_update do
        change cascade_update(:posts,
                 return_notifications?: true,
                 action: :no_notification_update
               )
      end
    end

    relationships do
      has_many :posts, Test.Post, public?: true
    end

    code_interface do
      define :create
      define :update
      define :update_with_atomic_upgrade
      define :no_notification_update
      define :read
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets, notifiers: [Test.Notifier]

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      read :custom_read do
        pagination keyset?: true, required?: false

        prepare fn query, _ ->
          Agent.update(
            Test.Agent,
            &%{&1 | custom_read_used: true}
          )

          query
        end
      end

      update :update do
        primary? true
        require_atomic? false
        skip_unknown_inputs [:content]

        change cascade_update(:comments,
                 copy_inputs: [:name]
               )

        change cascade_update(:summary,
                 copy_inputs: [:name, :content]
               )

        change before_action(fn changeset, _ ->
                 Agent.update(
                   Test.Agent,
                   &%{&1 | updates: MapSet.put(&1.updates, changeset.data.id)}
                 )

                 changeset
               end)
      end

      update :update_with_atomic_upgrade do
        atomic_upgrade_with :custom_read
        require_atomic? false
        skip_unknown_inputs [:content]
      end

      update :no_notification_update do
      end
    end

    relationships do
      has_many :comments, Test.Comment, public?: true
      has_one :summary, Test.Summary, public?: true
      belongs_to :author, Test.Author, public?: true, attribute_writable?: true
    end

    code_interface do
      define :create
      define :update
      define :read
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, create: :*]

      update :update do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      belongs_to :post, Test.Post, public?: true, attribute_writable?: true
    end

    code_interface do
      define :create
      define :update
      define :read
    end
  end

  defmodule Summary do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      default_accept :*
      defaults [:read, create: :*]

      update :update do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :content, :string, public?: true
    end

    relationships do
      belongs_to :post, Test.Post, public?: true, attribute_writable?: true
    end

    code_interface do
      define :create
      define :update
      define :read
    end
  end

  setup do
    {:ok, pid} =
      start_supervised(
        {Agent,
         fn -> %{updates: MapSet.new(), notifications: MapSet.new(), custom_read_used: false} end}
      )

    Process.register(pid, Test.Agent)

    :ok
  end

  test "when updating an author with name & content, all downstream relationships are also updated" do
    author = Author.create!(%{})
    post1 = Post.create!(%{author_id: author.id})
    post2 = Post.create!(%{author_id: author.id})

    comment1 = Comment.create!(%{post_id: post1.id})
    comment2 = Comment.create!(%{post_id: post1.id})

    summary = Summary.create!(%{post_id: post1.id})

    name = "Ash Framework"
    content = "Is great!"

    # Cascade the values down the chain...
    author |> Ash.update!(%{name: name, content: content})

    a = Ash.get!(Author, author.id)
    p = Ash.get!(Post, post1.id)
    p2 = Ash.get!(Post, post2.id)
    c1 = Ash.get!(Comment, comment1.id)
    c2 = Ash.get!(Comment, comment2.id)
    s1 = Ash.get!(Summary, summary.id)

    assert ^name = a.name
    assert ^name = p.name
    assert ^name = p2.name
    assert ^name = c1.name
    assert ^name = c2.name
    assert ^name = s1.name
    assert ^content = s1.content
  end

  test "wrong relationship in cascade should error" do
    author = Author.create!(%{})
    Post.create!(%{author_id: author.id})

    catch_error(
      author
      |> Ash.Changeset.for_update(:wrong_relationship_cascade, %{name: "Ash"})
      |> Ash.update!()
    )
  end

  test "updated records are notified" do
    author = Author.create!(%{})

    post_ids =
      1..Enum.random(3..25)
      |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)
      |> MapSet.new(& &1.id)

    name = "Ash Framework"
    author |> Ash.update!(%{name: name})

    notified_ids = Agent.get(Test.Agent, & &1.notifications)

    assert MapSet.equal?(post_ids, notified_ids)
  end

  test "does not error when notifications are requested but none are returned - bulk" do
    author = Author.create!(%{})

    1..Enum.random(3..5)
    |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)

    Ash.bulk_update!([author], :no_notification_update, %{name: "Ash Framework"})

    assert [_ | _] = Post.read!()
    assert [_ | _] = Author.read!()
  end

  test "does not error when notifications are requested but none are returned - single" do
    author = Author.create!(%{})

    1..Enum.random(3..5)
    |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)

    Author.no_notification_update!(author, %{name: "Ash Framework"})

    assert [_ | _] = Post.read!()
    assert [_ | _] = Author.read!()
  end

  test "does not error when there is nothing to cascade update" do
    author = Author.create!(%{})
    name = "Ash Framework"
    Author.update!(author, %{name: name})

    a2 = Ash.get!(Author, author.id)
    assert ^name = a2.name
  end

  test "uses atomic_upgrade_with action when specified" do
    author = Author.create!(%{})
    post1 = Post.create!(%{author_id: author.id})
    post2 = Post.create!(%{author_id: author.id})

    name = "Ash Framework"
    content = "Is great!"

    author |> Author.update_with_atomic_upgrade!(%{name: name, content: content})

    assert Agent.get(Test.Agent, & &1.custom_read_used) == true

    a = Ash.get!(Author, author.id)
    p = Ash.get!(Post, post1.id)
    p2 = Ash.get!(Post, post2.id)

    assert ^name = a.name
    assert ^name = p.name
    assert ^name = p2.name
  end
end
