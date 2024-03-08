defmodule Ash.Flow.AuthorizationTest do
  use ExUnit.Case

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
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

  defmodule DestroyPost do
    use Ash.Flow

    flow do
      domain(Domain)

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
    post = Post |> Ash.Changeset.for_create(:create) |> Ash.create!()

    %{post: post}
  end

  test "Ash.destroy should fail when actor is not admin", %{post: post} do
    assert_raise Ash.Error.Forbidden, fn ->
      post |> Ash.destroy!(actor: %{admin: false})
    end
  end

  test "Ash.destroy should succeed when actor is admin", %{post: post} do
    post |> Ash.destroy!(actor: %{admin: true})
  end

  test "flow should fail when actor is not admin", %{post: post} do
    assert %Ash.Flow.Result{valid?: false} =
             DestroyPost.run(post.id, authorize?: true, actor: %{admin: false})
  end

  test "flow should succeed when actor is admin", %{post: post} do
    assert %Ash.Flow.Result{valid?: true, result: %{destroy_post: _}} =
             DestroyPost.run(post.id, authorize?: true, actor: %{admin: true})
  end
end
