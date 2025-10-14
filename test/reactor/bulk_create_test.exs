# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkCreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      defaults [:read, create: :*]
    end
  end

  defmodule BulkCreateReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :post_attrs

    bulk_create :create_posts, Post, :create do
      initial(input(:post_attrs))
    end
  end

  test "it can create a bunch of records all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    post_attrs =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}"})

    assert {:ok, _} =
             Reactor.run(BulkCreateReactor, %{post_attrs: post_attrs}, %{}, async?: false)

    created_posts = Ash.read!(Post, action: :read)

    assert length(created_posts) == how_many
  end
end
