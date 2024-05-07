defmodule Ash.Test.Resource.Change.CascadeDestroy do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain
  alias Ash.Test.Resource.Change.CascadeDestroy, as: Test

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      if notification.action.type == :destroy do
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
        change cascade_destroy(:posts, notify?: true)
      end
    end

    relationships do
      has_many :posts, Test.Post, public?: true
    end

    code_interface do
      define :create
      define :destroy
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
      start_supervised({Agent, fn -> %{destroys: MapSet.new(), notifications: MapSet.new()} end})

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
end
