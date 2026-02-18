# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ThroughTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

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
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :source_id, :uuid do
        public?(true)
      end
    end

    relationships do
      belongs_to :destination, Post, public?: true
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
      default_accept :*
      defaults [:read, :destroy, update: :*]

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
      has_many :source_post_links, PostLink do
        public?(true)
        destination_attribute :source_id
        domain(OtherDomain)
      end

      has_many :destination_post_links, PostLink do
        public?(true)
        destination_attribute :destination_id
        domain(OtherDomain)
      end

      has_many :source_posts, __MODULE__, through: [:destination_post_links, :source]

      has_many :destination_posts, __MODULE__ do
        through [:source_post_links, :destination]
        sort title: :asc
      end

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

  describe "manage_relationship on through relationships" do
    test "emits a compile-time error when manage_relationship targets a through relationship" do
      output =
        capture_io(:stderr, fn ->
          defmodule InvalidPostWithThroughManage do
            use Ash.Resource,
              domain: Ash.Test.Actions.ManyToManyTest.OtherDomain,
              data_layer: Ash.DataLayer.Ets

            actions do
              create :create do
                argument :related, {:array, :map}
                change manage_relationship(:related, :through_linked_posts, type: :create)
              end
            end

            attributes do
              uuid_primary_key :id
            end

            relationships do
              has_many :post_links, Ash.Test.Actions.ManyToManyTest.PostLink,
                destination_attribute: :source_id,
                domain: Ash.Test.Actions.ManyToManyTest.OtherDomain

              has_many :through_linked_posts, __MODULE__, through: [:post_links, :destination]
            end
          end
        end)

      assert output =~
               "Cannot use manage_relationship on through_linked_posts, because it has a through path"

      assert output =~ "Relationships with through paths are read-only."
    end
  end

  describe "in separate domains" do
    test "loads through relationship" do
      {title1, title2} = {"foo", "bar"}

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "buz",
          linked_posts: [%{title: title1}, %{title: title2}]
        })
        |> Ash.create!()
        |> Ash.load!(:destination_posts)

      assert [%{title: ^title2}, %{title: ^title1}] = post.destination_posts
    end

    test "raises error if relationship is not defined" do
      assert_raise Ash.Error.Invalid,
                   ~r/No such relationship source for resource Ash.Test.Actions.ThroughTest.PostLink/,
                   fn ->
                     Post
                     |> Ash.Changeset.for_create(:create, %{
                       title: "buz",
                       linked_posts: [%{title: "foo"}, %{title: "bar"}]
                     })
                     |> Ash.create!()
                     |> Ash.load!(:source_posts)
                   end
    end
  end
end
