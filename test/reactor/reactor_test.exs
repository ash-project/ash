defmodule Ash.Test.ReactorTest do
  @moduledoc false
  use ExUnit.Case, async: false
  use Mimic
  setup :set_mimic_global

  test "it can be used directly" do
    defmodule DirectReactor do
      @moduledoc false
      use Ash.Reactor

      input :whom

      step :greet do
        argument :whom, input(:whom)
        run fn %{whom: whom} -> {:ok, "Hello, #{whom}!"} end
      end
    end

    assert {:ok, "Hello, Marty!"} = Reactor.run(DirectReactor, %{whom: "Marty"})
  end

  test "notifications are published when the reactor is successful" do
    defmodule Post do
      @moduledoc false
      use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

      ets do
        private? true
      end

      attributes do
        uuid_primary_key :id
        attribute :title, :string, allow_nil?: false, public?: true
      end

      actions do
        default_accept :*
        defaults [:destroy, create: :*]
      end
    end

    defmodule NotifyingReactor do
      @moduledoc false
      use Ash.Reactor

      input :title

      ash do
        default_domain(Ash.Test.Domain)
      end

      create :create_post, Ash.Test.ReactorTest.Post do
        inputs(%{title: input(:title)})
      end
    end

    expect(Ash.Reactor.Notifications, :publish, fn notifications ->
      assert [
               %Ash.Notifier.Notification{
                 resource: Ash.Test.ReactorTest.Post,
                 action: %{name: :create}
               }
             ] = notifications

      []
    end)

    assert {:ok, _post} = Reactor.run(NotifyingReactor, %{title: "Title"})
  end
end
