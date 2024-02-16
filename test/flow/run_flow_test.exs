defmodule Ash.Flow.RunFlowTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  defmodule CreatePost do
    use Ash.Flow

    flow do
      api Api

      returns create_post: :post
    end

    steps do
      create :create_post, Post, :create do
        input %{}
      end
    end
  end

  test "CreatePost" do
    assert %{valid?: true, result: %{post: post}} = CreatePost.run(%{})
  end

  test "CreateTwoPosts" do
    # COMPILE ERROR with
    # ** (Spark.Error.DslError) [nil]
    # create_two_posts:
    #  Must have at least one step.

    defmodule CreateTwoPosts do
      use Ash.Flow

      flow do
        api Api

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
