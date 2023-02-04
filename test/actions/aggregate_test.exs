defmodule Ash.Test.Actions.AggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string

      attribute :public, :boolean do
        default false
      end
    end

    policies do
      policy always() do
        authorize_if expr(public == true)
      end
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

  test "allows counting records" do
    assert %{count: 0} = Api.aggregate!(Post, {:count, :count})

    Post
    |> Ash.Changeset.for_create(:create, %{title: "title"})
    |> Api.create!()

    assert %{count: 1} = Api.aggregate!(Post, {:count, :count})

    Post
    |> Ash.Changeset.for_create(:create, %{title: "title"})
    |> Api.create!()

    assert %{count: 2} = Api.aggregate!(Post, {:count, :count})
  end

  test "runs authorization" do
    assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)

    Post
    |> Ash.Changeset.for_create(:create, %{title: "title"})
    |> Api.create!()

    assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)

    Post
    |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
    |> Api.create!()

    assert %{count: 1} = Api.aggregate!(Post, {:count, :count}, authorize?: true)
  end
end
