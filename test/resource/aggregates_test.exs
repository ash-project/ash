defmodule Ash.Test.Resource.AggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Aggregate
  alias Ash.Test.Domain, as: Domain

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :post_id, :uuid do
        public?(true)
      end
    end

    relationships do
      has_many :likes, Like, destination_attribute: :comment_id, public?: true
    end
  end

  defmodule Like do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :comment_id, :uuid do
        public?(true)
      end
    end
  end

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "aggregates are persisted on the resource properly" do
      defposts do
        aggregates do
          count :count_of_comments, :comments, public?: true
          count :another_count_but_private, :comments
        end

        relationships do
          has_many :comments, Comment, destination_attribute: :post_id, public?: true
        end
      end

      assert [
               %Aggregate{
                 name: :count_of_comments,
                 kind: :count,
                 relationship_path: [:comments],
                 public?: true
               },
               %Aggregate{
                 name: :another_count_but_private,
                 kind: :count,
                 relationship_path: [:comments],
                 public?: false
               }
             ] = Ash.Resource.Info.aggregates(Post)

      assert [
               %Aggregate{name: :count_of_comments}
             ] = Ash.Resource.Info.public_aggregates(Post)

      assert %Aggregate{name: :another_count_but_private} =
               Ash.Resource.Info.aggregate(Post, :another_count_but_private)

      assert nil == Ash.Resource.Info.public_aggregate(Post, :another_count_but_private)

      assert nil == Ash.Resource.Info.aggregate(Post, :totally_legit_aggregate)
    end

    test "Aggregate descriptions are allowed" do
      defposts do
        aggregates do
          count :count_of_comments, :comments,
            description: "require one of name/contents",
            public?: true
        end

        relationships do
          has_many :comments, Comment, destination_attribute: :post_id, public?: true
        end
      end

      assert [
               %Ash.Resource.Aggregate{description: "require one of name/contents"}
             ] = Ash.Resource.Info.aggregates(Post)
    end

    test "aggregates field should be calculation or attribute on the resource" do
      assert_raise Spark.Error.DslError, fn ->
        defposts do
          aggregates do
            sum :sum_of_comment_likes, :comments, :likes do
              public? true
            end
          end

          relationships do
            has_many :comments, Comment, destination_attribute: :post_id, public?: true
          end
        end
      end
    end
  end
end
