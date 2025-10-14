# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Change.CascadeDestroy do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain
  alias Ash.Test.Resource.Change.CascadeDestroy, as: Test

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      if notification.action.name == :destroy do
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
    end

    actions do
      defaults [:read, create: :*]

      destroy :destroy do
        primary? true
        change cascade_destroy(:posts, return_notifications?: true)
      end

      destroy :destroy_with_atomic_upgrade do
        change cascade_destroy(:posts,
                 action: :destroy_with_atomic_upgrade,
                 return_notifications?: true
               )
      end

      destroy :no_notification_destroy do
        change cascade_destroy(:posts,
                 return_notifications?: true,
                 action: :no_notification_destroy
               )
      end
    end

    relationships do
      has_many :posts, Test.Post, public?: true
    end

    code_interface do
      define :create
      define :destroy
      define :destroy_with_atomic_upgrade
      define :no_notification_destroy
      define :read
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets, notifiers: [Test.Notifier]

    attributes do
      uuid_primary_key :id
    end

    actions do
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

      destroy :destroy do
        primary? true
        require_atomic? false

        change before_action(fn changeset, _ ->
                 Agent.update(
                   Test.Agent,
                   &%{&1 | destroys: MapSet.put(&1.destroys, changeset.data.id)}
                 )

                 changeset
               end)
      end

      destroy :destroy_with_atomic_upgrade do
        atomic_upgrade_with :custom_read
        require_atomic? false

        change before_action(fn changeset, _ ->
                 Agent.update(
                   Test.Agent,
                   &%{&1 | destroys: MapSet.put(&1.destroys, changeset.data.id)}
                 )

                 changeset
               end)
      end

      destroy :no_notification_destroy do
      end
    end

    relationships do
      belongs_to :author, Test.Author, public?: true, attribute_writable?: true
    end

    code_interface do
      define :create
      define :read
    end
  end

  setup do
    {:ok, pid} =
      start_supervised(
        {Agent,
         fn -> %{destroys: MapSet.new(), notifications: MapSet.new(), custom_read_used: false} end}
      )

    Process.register(pid, Test.Agent)

    :ok
  end

  test "when destroying an author, all their posts area also destroyed" do
    author = Author.create!(%{})

    post_ids =
      1..Enum.random(3..25)
      |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)
      |> MapSet.new(& &1.id)

    Author.destroy!(author)

    deleted_ids = Agent.get(Test.Agent, & &1.destroys)

    assert MapSet.equal?(post_ids, deleted_ids)

    assert [] = Post.read!()
    assert [] = Author.read!()
  end

  test "destroyed records are notified" do
    author = Author.create!(%{})

    post_ids =
      1..Enum.random(3..25)
      |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)
      |> MapSet.new(& &1.id)

    Author.destroy!(author)

    notified_ids = Agent.get(Test.Agent, & &1.notifications)

    assert MapSet.equal?(post_ids, notified_ids)
  end

  test "does not error when notifications are requested but none are returned - bulk" do
    author = Author.create!(%{})

    1..Enum.random(3..5)
    |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)

    Ash.bulk_destroy([author], :no_notification_destroy, %{}, return_errors?: true)

    assert [] = Post.read!()
    assert [] = Author.read!()
  end

  test "does not error when notifications are requested but none are returned - single" do
    author = Author.create!(%{})

    1..Enum.random(3..5)
    |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)

    Author.no_notification_destroy!(author)

    assert [] = Post.read!()
    assert [] = Author.read!()
  end

  test "does not error when there is nothing to cascade destroy - resource provided" do
    author = Author.create!(%{})
    Author.destroy!(author)
    assert [] = Author.read!()
  end

  test "does not error when there is nothing to cascade destroy - ID provided" do
    author = Author.create!(%{})
    Author.destroy!(author.id)
    assert [] = Author.read!()
  end

  test "uses atomic_upgrade_with action when specified" do
    author = Author.create!(%{})

    post_ids =
      1..3
      |> Enum.map(fn _ -> Post.create!(%{author_id: author.id}) end)
      |> MapSet.new(& &1.id)

    Author.destroy_with_atomic_upgrade!(author)

    assert Agent.get(Test.Agent, & &1.custom_read_used) == true

    deleted_ids = Agent.get(Test.Agent, & &1.destroys)
    assert MapSet.equal?(post_ids, deleted_ids)

    assert [] = Post.read!()
    assert [] = Author.read!()
  end
end
