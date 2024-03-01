defmodule Ash.Test.Resource.InfoTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource
  alias Ash.Resource.Info
  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :metadata, :map do
        public?(true)
      end

      attribute :authors, {:array, :string} do
        public?(true)
      end

      attribute :points, :integer
    end

    aggregates do
      count :count_of_comments, :comments do
        public? true
      end
    end

    relationships do
      many_to_many :tags, Tag do
        through TagOnPost
        public?(true)
        source_attribute_on_join_resource(:vendor_id)
        destination_attribute_on_join_resource(:shipping_method_id)
      end

      has_many :comments, Comment, destination_attribute: :post_id, public?: true
      has_one :private, PostPrivate, destination_attribute: :post_id, public?: true
    end
  end

  defmodule Tag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :label, :string do
        public?(true)
      end
    end
  end

  defmodule TagOnPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
      end

      belongs_to :tag, Tag do
        public?(true)
      end
    end
  end

  defmodule PostPrivate do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :notes, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :subjects, {:array, :string} do
        public?(true)
      end

      attribute :post_id, :uuid do
        public?(true)
      end
    end

    aggregates do
      first :post_title, :post, :title do
        public? true
      end

      first :post_authors, :post, :authors do
        public? true
      end
    end

    calculations do
      calculate :formatted_post_title, :string, expr("Post title: " <> post_title),
        public?: true,
        load: [:post_title]
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
      end
    end
  end

  describe "resource info" do
    test "get only public fields" do
      assert %Resource.Attribute{name: :title} = Info.public_field(Post, :title)
      assert nil == Info.public_field(Post, :points)

      assert [
               :authors,
               :comments,
               :contents,
               :count_of_comments,
               :id,
               :metadata,
               :private,
               :tags,
               :title
             ] = Info.public_fields(Post) |> Enum.map(& &1.name) |> Enum.sort()
    end

    test "get any fields" do
      assert %Resource.Attribute{name: :title} = Info.public_field(Post, :title)
      assert %Resource.Attribute{name: :points} = Info.field(Post, :points)

      assert [
               :authors,
               :comments,
               :contents,
               :count_of_comments,
               :id,
               :metadata,
               :points,
               :private,
               :tags,
               :tags_join_assoc,
               :title
             ] = Info.fields(Post) |> Enum.map(& &1.name) |> Enum.sort()
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

    test "get datalayer from resource" do
      assert Ash.DataLayer.Ets == Info.data_layer(Post)
    end
  end
end
