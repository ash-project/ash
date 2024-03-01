defmodule Ash.Test.Actions.ManyToManyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule OtherDomain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule PostLink do
    use Ash.Resource,
      domain: OtherDomain,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :source_id, :uuid do
        public?(true)
      end

      attribute :destination_id, :uuid do
        public?(true)
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        primary? true

        argument :linked_posts, {:array, :map}

        change manage_relationship(:linked_posts, type: :create)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end
    end

    relationships do
      has_many :post_links, PostLink do
        public?(true)
        destination_attribute :source_id
        domain(OtherDomain)
      end

      many_to_many :linked_posts, __MODULE__ do
        public?(true)
        through PostLink
        join_relationship :post_links
        source_attribute_on_join_resource :source_id
        destination_attribute_on_join_resource :destination_id
      end
    end
  end

  describe "in separate registries" do
    test "it allows managing without raising an error" do
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "buz",
        linked_posts: [%{title: "foo"}, %{title: "bar"}]
      })
      |> Domain.create!()
    end
  end
end
