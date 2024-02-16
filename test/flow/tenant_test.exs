defmodule Ash.Flow.TenantTest do
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

      read :get_by_id do
        get_by(:id)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :tenant, :string
    end

    multitenancy do
      global? true
      strategy :attribute
      attribute :tenant
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

  test "honors Ash.set_tenant" do
    foo_post =
      Post
      |> Ash.Changeset.for_create(:create, %{tenant: "foo"})
      |> Api.create!()

    Ash.set_tenant("bar")
    assert %Ash.Flow.Result{valid?: false} = DestroyPost.run(foo_post.id)

    Ash.set_tenant("foo")
    assert %Ash.Flow.Result{valid?: true} = DestroyPost.run(foo_post.id)
  end

  test "honors tenant in opts" do
    foo_post =
      Post
      |> Ash.Changeset.for_create(:create, %{tenant: "foo"})
      |> Api.create!()

    assert %Ash.Flow.Result{valid?: false} = DestroyPost.run(foo_post.id, tenant: "bar")
    assert %Ash.Flow.Result{valid?: true} = DestroyPost.run(foo_post.id, tenant: "foo")
  end
end
