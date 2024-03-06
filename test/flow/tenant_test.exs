defmodule Ash.Flow.TenantTest do
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

      read :get_by_id do
        get_by(:id)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :tenant, :string do
        public?(true)
      end
    end

    multitenancy do
      global? true
      strategy :attribute
      attribute :tenant
    end
  end

  defmodule GetPost do
    use Ash.Flow.Step

    def run(%{id: post_id}, _opts, context) do
      opts = context |> Ash.context_to_opts()

      Post
      |> Ash.Query.for_read(:get_by_id, %{id: post_id}, opts)
      |> Domain.read_one(not_found_error?: true)
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
      custom :get_post, GetPost do
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
      |> Domain.create!()

    Ash.set_tenant("bar")

    assert %Ash.Flow.Result{valid?: false} =
             DestroyPost.run(foo_post.id)

    Ash.set_tenant("foo")
    assert %Ash.Flow.Result{valid?: true} = DestroyPost.run(foo_post.id)
  end

  test "honors tenant in opts" do
    foo_post =
      Post
      |> Ash.Changeset.for_create(:create, %{tenant: "foo"})
      |> Domain.create!()

    assert %Ash.Flow.Result{valid?: false} =
             DestroyPost.run(foo_post.id, tenant: "bar")

    assert %Ash.Flow.Result{valid?: true} = DestroyPost.run(foo_post.id, tenant: "foo")
  end
end
