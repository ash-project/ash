defmodule Ash.Test.Type.AutoTypePubSubTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule PubSubModule do
    @moduledoc false
    def broadcast(topic, event, payload) do
      send(
        Application.get_env(__MODULE__, :test_pid),
        {:pub_sub, topic, event, payload}
      )
    end
  end

  defmodule PubSubResource do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Ash.Notifier.PubSub]

    ets do
      private?(true)
    end

    pub_sub do
      module PubSubModule
      prefix "resource"

      publish :create, "created", transform: :display_name
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string, public?: true
      attribute :last_name, :string, public?: true
    end

    calculations do
      calculate :display_name, :string, expr(first_name <> " " <> last_name) do
        public?(true)
      end
    end
  end

  setup do
    Application.put_env(PubSubModule, :test_pid, self())
    :ok
  end

  describe "pub_sub with calculation transform" do
    test "calculation transform broadcasts calculation result" do
      PubSubResource
      |> Ash.Changeset.for_create(:create, %{first_name: "John", last_name: "Doe"})
      |> Ash.create!()

      assert_receive {:pub_sub, "resource:created", "create", value}
      assert value == "John Doe"
    end
  end
end
