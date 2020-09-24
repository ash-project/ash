defmodule Ash.Test.Resource.AggregatesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Comment do
    @moduledoc false
    use Ash.Resource

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :post_id, :uuid
    end
  end

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "aggregates are persisted on the resource properly" do
      defposts do
        aggregates do
          count :count_of_comments, :comments
        end

        relationships do
          has_many :comments, Comment, destination_field: :post_id
        end
      end

      assert [
               %Ash.Resource.Aggregate{
                 name: :count_of_comments,
                 relationship_path: [:comments]
               }
             ] = Ash.Resource.aggregates(Post)
    end

    test "Aggregate descriptions are allowed" do
      defposts do
        aggregates do
          count :count_of_comments, :comments, description: "require one of name/contents"
        end

        relationships do
          has_many :comments, Comment, destination_field: :post_id
        end
      end

      assert [
               %Ash.Resource.Aggregate{description: "require one of name/contents"}
             ] = Ash.Resource.aggregates(Post)
    end
  end
end
