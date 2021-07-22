defmodule Ash.Test.Resource.InfoTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource
  alias Ash.Resource.Info

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :metadata, :map
      attribute :authors, {:array, :string}
      attribute :points, :integer, private?: true
    end

    aggregates do
      count :count_of_comments, :comments
    end

    relationships do
      many_to_many :tags, Tag do
        through TagOnPost
        source_field_on_join_table :vendor_id
        destination_field_on_join_table :shipping_method_id
      end

      has_many :comments, Comment, destination_field: :post_id
      has_one :private, PostPrivate, destination_field: :post_id
    end
  end

  defmodule Tag do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :label, :string
    end
  end

  defmodule TagOnPost do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :post, Post
      belongs_to :tag, Tag
    end
  end

  defmodule PostPrivate do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :notes, :string
    end

    relationships do
      belongs_to :post, Post
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :subjects, {:array, :string}
      attribute :post_id, :uuid
    end

    aggregates do
      first :post_title, :post, :title
      first :post_authors, :post, :authors
    end

    calculations do
      calculate :formatted_post_title, :string, expr("Post title: " <> post_title),
        load: [:post_title]
    end

    relationships do
      belongs_to :post, Post
    end
  end

  describe "resource info" do
    test "get only public fields" do
      assert %Resource.Attribute{name: :title} = Info.public_field(Post, :title)
      assert nil == Info.public_field(Post, :points)
    end

    test "get any fields" do
      assert %Resource.Attribute{name: :title} = Info.public_field(Post, :title)
      assert %Resource.Attribute{name: :points} = Info.field(Post, :points)
    end

    test "determine if field is sortable" do
      assert true == Info.sortable?(Post, :title)
      assert true == Info.sortable?(Post, :points)
      assert false == Info.sortable?(Post, :points, include_private?: false)
      assert false == Info.sortable?(Post, :authors)
      assert false == Info.sortable?(Post, :tags)
      assert false == Info.sortable?(Post, :metadata)
      assert false == Info.sortable?(Post, :comments)
      assert false == Info.sortable?(Post, :private)
      assert false == Info.sortable?(Comment, :post)
      assert true == Info.sortable?(Post, :count_of_comments)
      assert false == Info.sortable?(Post, :count_of_comments, pagination_type: :keyset)
      assert true == Info.sortable?(Comment, :post_title)
      assert true == Info.sortable?(Comment, :formatted_post_title)
      assert false == Info.sortable?(Comment, :post_authors)
      assert false == Info.sortable?(Comment, :formatted_post_title, pagination_type: :keyset)
    end
  end
end
