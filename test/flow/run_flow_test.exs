defmodule Ash.Flow.RunFlowTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

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
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule CreatePost do
    use Ash.Flow

    flow do
      domain(Domain)

      returns create_post: :post
    end

    steps do
      create :create_post, Post, :create do
        input %{}
      end
    end
  end

  test "CreatePost" do
    assert %{valid?: true, result: %{post: _}} = CreatePost.run(%{})
  end

  test "CreateTwoPosts" do
    defmodule CreateTwoPosts do
      use Ash.Flow

      flow do
        domain(Domain)

        returns create_two_posts: :posts
      end

      steps do
        map :create_two_posts, range(1, 2) do
          run_flow :create_post, CreatePost do
            input %{}
          end
        end
      end
    end

    CreateTwoPosts.run(%{})
  end
end
