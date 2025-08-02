defmodule Ash.Test.Resource.InfoTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Spark.Dsl
  require Spark.Dsl.Extension
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
      many_to_many :tags, Ash.Test.Resource.InfoTest.Tag do
        through Ash.Test.Resource.InfoTest.TagOnPost
        public?(true)
        source_attribute_on_join_resource(:tag_id)
        destination_attribute_on_join_resource(:post_id)
      end

      has_many :comments, Ash.Test.Resource.InfoTest.Comment,
        destination_attribute: :post_id,
        public?: true

      has_one :private, Ash.Test.Resource.InfoTest.PostPrivate,
        destination_attribute: :post_id,
        public?: true
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

      calculate :field_calculation, :string, expr("Field calc"),
        public?: true,
        field?: true

      calculate :non_field_calculation, :string, expr("Non-field calc"),
        public?: true,
        field?: false

      calculate :default_field_calculation, :string, expr("Default field calc"), public?: true
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

    test "get relationship fields" do
      assert Spark.Dsl.Extension.get_entities(Comment, [:post])

      comment = Info.public_relationship(Post, [:comments]).destination
      assert Spark.Dsl.Extension.get_entities(comment, [:post])

      assert %Resource.Relationships.ManyToMany{name: :tags} =
               Info.public_relationship(Post, :tags)

      assert %Resource.Relationships.BelongsTo{name: :post} =
               Info.public_relationship(Post, [:comments, :post])
    end

    test "fields filters calculations by field? attribute" do
      # Get all fields from Comment resource
      all_fields = Info.fields(Comment)
      field_names = Enum.map(all_fields, & &1.name) |> Enum.sort()

      # Should include attributes, aggregates, and relationships (which don't have field? attribute)
      assert :id in field_names
      assert :subjects in field_names
      assert :post_id in field_names
      # aggregate
      assert :post_title in field_names
      # aggregate
      assert :post_authors in field_names
      # relationship
      assert :post in field_names

      # Should include calculations with field?: true or default (true)
      assert :formatted_post_title in field_names
      assert :field_calculation in field_names
      assert :default_field_calculation in field_names

      # Should exclude calculations with field?: false
      refute :non_field_calculation in field_names

      # Verify that we're getting all types of entities except calculations with field?: false
      # Total should be: 3 attributes + 2 aggregates + 1 relationship + 3 field calculations = 9
      assert length(all_fields) == 9
    end
  end
end
