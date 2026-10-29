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
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    ets do
      private? true
    end

    policies do
      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if expr(visible == true)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :source_id, :uuid do
        public?(true)
      end

      attribute :visible, :boolean, public?: true, default: true
    end

    relationships do
      belongs_to :destination, Ash.Test.Actions.ThroughTest.Post, public?: true
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

      has_many :through_linked_posts, __MODULE__,
        through: [:post_links, :destination],
        public?: true
    end
  end

  defmodule Product do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      has_one :media_set, Ash.Test.Actions.ThroughTest.MediaSet do
        public? true
        destination_attribute :product_id
      end

      has_many :media_sets, Ash.Test.Actions.ThroughTest.MediaSet do
        public? true
        destination_attribute :product_id
      end

      has_one :hidden_media_set, Ash.Test.Actions.ThroughTest.MediaSet do
        public? true
        destination_attribute :product_id
        read_action :hidden
      end

      has_one :unassigned_media_set, Ash.Test.Actions.ThroughTest.MediaSet do
        public? true
        destination_attribute :product_id
        filter expr(is_nil(product_id))
      end

      has_one :parented_media_set, Ash.Test.Actions.ThroughTest.MediaSet do
        public? true
        destination_attribute :product_id
        filter expr(product_id == parent(id))
      end

      has_many :items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :items],
        public?: true

      has_many :featured_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :featured_items],
        public?: true

      has_many :item_tags, Ash.Test.Actions.ThroughTest.Tag,
        through: [:items, :tags],
        public?: true

      has_many :entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :entries],
        public?: true

      has_many :storefront_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :storefront_entries],
        public?: true

      has_many :hidden_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:hidden_media_set, :entries],
        public?: true

      has_many :unassigned_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:unassigned_media_set, :entries],
        public?: true

      has_many :entries_of_parented_set, Ash.Test.Actions.ThroughTest.Entry,
        through: [:parented_media_set, :entries],
        public?: true

      has_many :parented_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :parented_entries],
        public?: true

      has_many :filtered_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :filtered_entries],
        public?: true

      has_many :entry_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :entries, :item],
        public?: true

      has_one :an_entry_item, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :entries, :item],
        public?: true

      has_many :sorted_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :sorted_entries],
        public?: true

      has_many :limited_entries, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_set, :limited_entries],
        public?: true

      has_many :limited_entries_of_media_sets, Ash.Test.Actions.ThroughTest.Entry,
        through: [:media_sets, :limited_entries],
        public?: true

      has_many :limited_entry_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :limited_entries, :item],
        public?: true

      has_many :first_entry_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :first_entry, :item],
        public?: true

      has_many :parented_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :parented_entries, :parented_item],
        public?: true

      has_many :parented_m2m_items, Ash.Test.Actions.ThroughTest.Item,
        through: [:media_set, :parented_items],
        public?: true

      has_many :parented_slots, Ash.Test.Actions.ThroughTest.Slot,
        through: [:media_set, :parented_slots],
        public?: true
    end
  end

  defmodule MediaSet do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]

      read :hidden do
        filter expr(false)
      end
    end

    policies do
      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if actor_present()
        authorize_if accessing_from(Ash.Test.Actions.ThroughTest.Product, :media_set)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :product_id, :uuid, public?: true
    end

    relationships do
      has_many :entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
      end

      has_many :storefront_entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        read_action :storefront
      end

      has_many :parented_entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        filter expr(set_id == parent(id))
      end

      has_many :filtered_entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        filter expr(position > 1)
      end

      has_many :sorted_entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        sort position: :desc
      end

      has_many :limited_entries, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        sort position: :desc
        limit 2
      end

      has_one :first_entry, Ash.Test.Actions.ThroughTest.Entry do
        public? true
        destination_attribute :set_id
        from_many? true
        sort position: :asc
      end

      has_many :parented_slots, Ash.Test.Actions.ThroughTest.Slot do
        public? true
        destination_attribute :set_id
        filter expr(set_id == parent(id))
      end

      many_to_many :items, Ash.Test.Actions.ThroughTest.Item do
        public? true
        through Ash.Test.Actions.ThroughTest.Entry
        join_relationship :entries
        source_attribute_on_join_resource :set_id
        destination_attribute_on_join_resource :item_id
      end

      many_to_many :featured_items, Ash.Test.Actions.ThroughTest.Item do
        public? true
        through Ash.Test.Actions.ThroughTest.Entry
        join_relationship :entries
        source_attribute_on_join_resource :set_id
        destination_attribute_on_join_resource :item_id
        filter expr(featured == true)
      end

      many_to_many :parented_items, Ash.Test.Actions.ThroughTest.Item do
        public? true
        through Ash.Test.Actions.ThroughTest.Entry
        join_relationship :entries
        source_attribute_on_join_resource :set_id
        destination_attribute_on_join_resource :item_id
        filter expr(not is_nil(parent(id)))
      end
    end
  end

  defmodule Entry do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]

      read :storefront do
        filter expr(position > 1)
      end
    end

    policies do
      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if actor_present()
        authorize_if accessing_from(Ash.Test.Actions.ThroughTest.Product, :entries)
        authorize_if accessing_from(Ash.Test.Actions.ThroughTest.Product, :storefront_entries)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :set_id, :uuid, public?: true
      attribute :item_id, :uuid, public?: true
      attribute :position, :integer, public?: true
    end

    relationships do
      has_one :item, Ash.Test.Actions.ThroughTest.Item do
        public? true
        source_attribute :item_id
        destination_attribute :id
      end

      has_one :parented_item, Ash.Test.Actions.ThroughTest.Item do
        public? true
        source_attribute :item_id
        destination_attribute :id
        filter expr(id == parent(item_id))
      end
    end
  end

  defmodule Slot do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      attribute :set_id, :uuid, primary_key?: true, allow_nil?: false, public?: true
      attribute :position, :integer, primary_key?: true, allow_nil?: false, public?: true
    end
  end

  defmodule Item do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :featured, :boolean, public?: true, default: false
    end

    relationships do
      has_many :tags, Ash.Test.Actions.ThroughTest.Tag do
        public? true
      end
    end
  end

  defmodule Tag do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :item_id, :uuid, public?: true
    end
  end

  defp create_products do
    for name <- ["a", "b"] do
      product = Ash.create!(Product, %{name: name}, authorize?: false)
      set = Ash.create!(MediaSet, %{product_id: product.id}, authorize?: false)

      for position <- 1..3 do
        item =
          Ash.create!(Item, %{name: "#{name}#{position}", featured: position == 1},
            authorize?: false
          )

        Ash.create!(Tag, %{name: "#{name}#{position}-tag", item_id: item.id}, authorize?: false)

        Ash.create!(
          Entry,
          %{set_id: set.id, item_id: item.id, position: position},
          authorize?: false
        )
      end

      product
    end
  end

  defp names_by_product(products, relationship) do
    Enum.map(products, fn product ->
      {product.name, product |> Map.get(relationship) |> Enum.map(& &1.name) |> Enum.sort()}
    end)
  end

  defp positions_by_product(products, relationship) do
    Enum.map(products, fn product ->
      {product.name, product |> Map.get(relationship) |> Enum.map(& &1.position) |> Enum.sort()}
    end)
  end

  describe "through relationships with a many_to_many step" do
    test "loads the many_to_many's destination records" do
      products = create_products() |> Ash.load!(:items, authorize?: false)

      assert names_by_product(products, :items) == [
               {"a", ["a1", "a2", "a3"]},
               {"b", ["b1", "b2", "b3"]}
             ]
    end

    test "applies the many_to_many's filter" do
      products = create_products() |> Ash.load!(:featured_items, authorize?: false)

      assert names_by_product(products, :featured_items) == [{"a", ["a1"]}, {"b", ["b1"]}]
    end

    test "loads a path through a nested through relationship ending in a many_to_many" do
      products = create_products() |> Ash.load!(:item_tags, authorize?: false)

      assert names_by_product(products, :item_tags) == [
               {"a", ["a1-tag", "a2-tag", "a3-tag"]},
               {"b", ["b1-tag", "b2-tag", "b3-tag"]}
             ]
    end
  end

  describe "through relationship paths" do
    test "loads a path of three steps" do
      products = create_products() |> Ash.load!(:entry_items, authorize?: false)

      assert names_by_product(products, :entry_items) == [
               {"a", ["a1", "a2", "a3"]},
               {"b", ["b1", "b2", "b3"]}
             ]
    end

    test "the last relationship's sort orders the destination" do
      products = create_products() |> Ash.load!(:sorted_entries, authorize?: false)

      assert Enum.map(products, fn product -> Enum.map(product.sorted_entries, & &1.position) end) ==
               [[3, 2, 1], [3, 2, 1]]
    end

    test "the last relationship's limit applies to each record it is reached from" do
      [product | _] = products = create_products()

      assert products
             |> Ash.load!(:limited_entries, authorize?: false)
             |> positions_by_product(:limited_entries) == [{"a", [2, 3]}, {"b", [2, 3]}]

      set = Ash.create!(MediaSet, %{product_id: product.id}, authorize?: false)

      for position <- 4..6 do
        Ash.create!(Entry, %{set_id: set.id, position: position}, authorize?: false)
      end

      product = Ash.load!(product, :limited_entries_of_media_sets, actor: %{id: "seller"})

      assert product.limited_entries_of_media_sets |> Enum.map(& &1.position) |> Enum.sort() ==
               [2, 3, 5, 6]
    end

    test "a limit of 1 on one record's load follows the path" do
      [product | _] = create_products()

      names =
        Enum.flat_map(0..5, fn offset ->
          query = Item |> Ash.Query.sort(:name) |> Ash.Query.limit(1) |> Ash.Query.offset(offset)

          product
          |> Ash.load!([entry_items: query], authorize?: false)
          |> Map.get(:entry_items)
          |> Enum.map(& &1.name)
        end)

      assert names == ["a1", "a2", "a3"]
    end

    test "a has_one loaded with a limit of 1 follows the path" do
      products =
        create_products()
        |> Ash.load!([an_entry_item: Ash.Query.limit(Item, 1)], authorize?: false)

      assert Enum.map(products, &String.first(&1.an_entry_item.name)) == ["a", "b"]
    end

    test "a middle relationship's limit applies to each record it is reached from" do
      products =
        create_products()
        |> Ash.load!([:limited_entry_items, :first_entry_items], authorize?: false)

      assert names_by_product(products, :limited_entry_items) == [
               {"a", ["a2", "a3"]},
               {"b", ["b2", "b3"]}
             ]

      assert names_by_product(products, :first_entry_items) == [{"a", ["a1"]}, {"b", ["b1"]}]
    end

    test "the last relationship's filter applies to the destination" do
      products = create_products() |> Ash.load!(:filtered_entries, authorize?: false)

      assert positions_by_product(products, :filtered_entries) == [
               {"a", [2, 3]},
               {"b", [2, 3]}
             ]
    end
  end

  describe "through relationship step options" do
    test "the last relationship's read action is used for the destination" do
      products =
        create_products() |> Ash.load!(:storefront_entries, actor: %{id: "seller"})

      assert positions_by_product(products, :storefront_entries) == [
               {"a", [2, 3]},
               {"b", [2, 3]}
             ]
    end

    test "a middle step is read with the read action of the relationship into it" do
      products = create_products() |> Ash.load!(:hidden_entries, actor: %{id: "seller"})

      assert positions_by_product(products, :hidden_entries) == [{"a", []}, {"b", []}]
    end

    test "a middle step is filtered by the relationship into it" do
      products = create_products() |> Ash.load!(:unassigned_entries, actor: %{id: "seller"})

      assert positions_by_product(products, :unassigned_entries) == [{"a", []}, {"b", []}]
    end

    test "a middle step is accessed from the relationship into it" do
      products = create_products() |> Ash.load!(:entries, actor: nil)

      assert positions_by_product(products, :entries) == [
               {"a", [1, 2, 3]},
               {"b", [1, 2, 3]}
             ]
    end
  end

  describe "through relationships with `parent/1` in a step's filter" do
    test "`parent/1` in the first relationship refers to the source record" do
      products = create_products() |> Ash.load!(:entries_of_parented_set, actor: %{id: "seller"})

      assert positions_by_product(products, :entries_of_parented_set) == [
               {"a", [1, 2, 3]},
               {"b", [1, 2, 3]}
             ]
    end

    test "`parent/1` in the last relationship refers to the record it is reached from" do
      products = create_products() |> Ash.load!(:parented_entries, actor: %{id: "seller"})

      assert positions_by_product(products, :parented_entries) == [
               {"a", [1, 2, 3]},
               {"b", [1, 2, 3]}
             ]
    end

    test "`parent/1` in middle and last relationships refers to the record each is reached from" do
      products = create_products() |> Ash.load!(:parented_items, actor: %{id: "seller"})

      assert names_by_product(products, :parented_items) == [
               {"a", ["a1", "a2", "a3"]},
               {"b", ["b1", "b2", "b3"]}
             ]
    end

    test "`parent/1` in a many_to_many step raises" do
      products = create_products()

      assert_raise Ash.Error.Unknown, ~r/many_to_many relationship `parented_items`/, fn ->
        Ash.load!(products, :parented_m2m_items, authorize?: false)
      end
    end

    test "`parent/1` in the last relationship raises when the destination has a composite primary key" do
      products = create_products()

      assert_raise Ash.Error.Unknown, ~r/composite primary key/, fn ->
        Ash.load!(products, :parented_slots, authorize?: false)
      end
    end
  end

  describe "through relationships" do
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

    test "loads through relationship in separate domains" do
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
      capture_io(:stderr, fn ->
        defmodule PostWithInvalidThrough do
          use Ash.Resource,
            domain: Ash.Test.Actions.ThroughTest.OtherDomain,
            data_layer: Ash.DataLayer.Ets

          ets do
            private? true
          end

          actions do
            default_accept :*
            defaults [:read, :destroy, create: :*, update: :*]
          end

          attributes do
            uuid_primary_key :id
            attribute :title, :string, public?: true
          end

          relationships do
            has_many :destination_post_links, Ash.Test.Actions.ThroughTest.PostLink,
              public?: true,
              destination_attribute: :destination_id,
              domain: Ash.Test.Actions.ThroughTest.OtherDomain

            has_many :source_posts, __MODULE__, through: [:destination_post_links, :source]
          end
        end
      end)

      post =
        Ash.Test.Actions.ThroughTest.PostWithInvalidThrough
        |> Ash.Changeset.for_create(:create, %{title: "buz"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid,
                   ~r/No such relationship source for resource Ash.Test.Actions.ThroughTest.PostLink/,
                   fn ->
                     Ash.load!(post, :source_posts)
                   end
    end

    test "filtering on a through relationship respects policies on the intermediate resource" do
      source =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "source"})
        |> Ash.create!()

      visible_dest =
        Post |> Ash.Changeset.for_create(:create, %{title: "visible_target"}) |> Ash.create!()

      hidden_dest =
        Post |> Ash.Changeset.for_create(:create, %{title: "hidden_target"}) |> Ash.create!()

      PostLink
      |> Ash.Changeset.for_create(:create, %{
        source_id: source.id,
        destination_id: visible_dest.id,
        visible: true
      })
      |> Ash.create!()

      PostLink
      |> Ash.Changeset.for_create(:create, %{
        source_id: source.id,
        destination_id: hidden_dest.id,
        visible: false
      })
      |> Ash.create!()

      # Without authorization, both through links are visible in filter
      result =
        Post
        |> Ash.Query.filter_input(through_linked_posts: [title: [eq: "hidden_target"]])
        |> Ash.read!(authorize?: false)

      assert length(result) == 1
      assert hd(result).id == source.id

      # With authorization, the invisible PostLink is filtered out,
      # so we can't reach hidden_target through it
      result =
        Post
        |> Ash.Query.filter_input(through_linked_posts: [title: [eq: "hidden_target"]])
        |> Ash.read!(authorize?: true, actor: %{})

      assert result == []

      # With authorization, visible_target is still reachable
      result =
        Post
        |> Ash.Query.filter_input(through_linked_posts: [title: [eq: "visible_target"]])
        |> Ash.read!(authorize?: true, actor: %{})

      assert length(result) == 1
      assert hd(result).id == source.id
    end

    test "exists filter works with a through relationship" do
      require Ash.Query

      source =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "source"})
        |> Ash.create!()

      dest =
        Post |> Ash.Changeset.for_create(:create, %{title: "target"}) |> Ash.create!()

      PostLink
      |> Ash.Changeset.for_create(:create, %{
        source_id: source.id,
        destination_id: dest.id,
        visible: true
      })
      |> Ash.create!()

      # exists on a through relationship should expand the path
      result =
        Post
        |> Ash.Query.filter(exists(through_linked_posts, title == "target"))
        |> Ash.read!(authorize?: false)

      assert length(result) == 1
      assert hd(result).id == source.id

      # Should return empty when no match
      result =
        Post
        |> Ash.Query.filter(exists(through_linked_posts, title == "nonexistent"))
        |> Ash.read!(authorize?: false)

      assert result == []
    end

    test "loading a through relationship respects policies on the intermediate resource" do
      source =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "source"})
        |> Ash.create!()

      visible_dest =
        Post |> Ash.Changeset.for_create(:create, %{title: "visible"}) |> Ash.create!()

      hidden_dest =
        Post |> Ash.Changeset.for_create(:create, %{title: "hidden"}) |> Ash.create!()

      PostLink
      |> Ash.Changeset.for_create(:create, %{
        source_id: source.id,
        destination_id: visible_dest.id,
        visible: true
      })
      |> Ash.create!()

      PostLink
      |> Ash.Changeset.for_create(:create, %{
        source_id: source.id,
        destination_id: hidden_dest.id,
        visible: false
      })
      |> Ash.create!()

      result = Ash.load!(source, :through_linked_posts, authorize?: false)
      assert length(result.through_linked_posts) == 2

      result = Ash.load!(source, :through_linked_posts, authorize?: true, actor: %{})
      assert length(result.through_linked_posts) == 1
      assert hd(result.through_linked_posts).title == "visible"
    end
  end
end
