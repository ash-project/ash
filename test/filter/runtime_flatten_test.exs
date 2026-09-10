# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Filter.RuntimeFlattenTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Tag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :post_id, :uuid, public?: true
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
      attribute :post_id, :uuid, public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :tags, Tag, public?: true
      has_many :comments, Comment, public?: true
    end
  end

  test "filtering over multiple to-many relationships does not materialize the full cross-product" do
    m = 300

    post = %Post{
      id: Ash.UUID.generate(),
      tags: Enum.map(0..(m - 1), fn i -> %Tag{id: Ash.UUID.generate(), name: "tag-#{i}"} end),
      comments:
        Enum.map(0..(m - 1), fn i -> %Comment{id: Ash.UUID.generate(), body: "body-#{i}"} end)
    }

    filter =
      Ash.Filter.parse!(Post, tags: [name: "tag-0"], comments: [body: "body-0"])

    {:reductions, before} = Process.info(self(), :reductions)
    assert {:ok, [_]} = Ash.Filter.Runtime.filter_matches(Domain, [post], filter)
    {:reductions, aft} = Process.info(self(), :reductions)

    assert aft - before < 1_000_000
  end
end
