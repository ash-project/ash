defmodule Ash.Flow.AuthorizationTest do
  use ExUnit.Case

  defmodule Post do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])

      read :get_by_id do
        get_by(:id)
      end
    end

    attributes do
      uuid_primary_key(:id)
    end

    policies do
      policy always() do
        authorize_if(action_type(:create))
        authorize_if(action_type(:read))
        authorize_if(actor_attribute_equals(:admin, true))
      end
    end
  end

  defmodule Registry do
    use Ash.Registry

    entries do
      entry(Post)
    end
  end

  defmodule Api do
    use Ash.Api

    authorization do
      authorize(:by_default)
    end

    resources do
      registry(Registry)
    end
  end

  defmodule DestroyPost do
    use Ash.Flow

    flow do
      api Api

      argument :post_id, :uuid do
        allow_nil? false
      end

      returns [:destroy_post]
    end

    steps do
      read :get_post, Post, :get_by_id do
        get? true
        input %{id: arg(:post_id)}
      end

      destroy :destroy_post, Post, :destroy do
        record result(:get_post)
      end
    end
  end

  setup do
    post = Post |> Ash.Changeset.for_create(:create) |> Api.create!()

    %{post: post}
  end

  test "Api.destroy should fail when actor is not admin", %{post: post} do
    Ash.set_actor(%{admin: false})

    assert_raise Ash.Error.Forbidden, fn ->
      post |> Api.destroy!()
    end
  end

  test "Api.destroy should succeed when actor is admin", %{post: post} do
    Ash.set_actor(%{admin: true})

    post |> Api.destroy!()
  end

  test "flow should fail when actor is not admin", %{post: post} do
    Ash.set_actor(%{admin: false})

    assert %Ash.Flow.Result{valid?: false} =
             DestroyPost.run(post.id, authorize?: true)
  end

  test "flow should succeed when actor is admin", %{post: post} do
    Ash.set_actor(%{admin: true})

    assert %Ash.Flow.Result{valid?: true, result: %{destroy_post: _}} =
             DestroyPost.run(post.id, authorize?: true)
  end
end
