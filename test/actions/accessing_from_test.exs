# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.AccessingFromTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :comments, Ash.Test.Actions.AccessingFromTest.Comment, public?: true
    end

    actions do
      defaults [:read, :create]
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :spam?, :boolean, allow_nil?: false, default: false, public?: true
    end

    relationships do
      belongs_to :post, Post, public?: true, attribute_writable?: true
    end

    preparations do
      # hide spam only when comments are loaded through a post
      prepare fn query, _context ->
        case query.context[:accessing_from] do
          %{source: Post, name: :comments} -> Ash.Query.filter(query, spam? == false)
          _ -> query
        end
      end
    end

    actions do
      defaults [:read, create: [:post_id, :spam?]]
    end
  end

  setup do
    post = Ash.create!(Post, %{})
    Ash.create!(Comment, %{post_id: post.id, spam?: false})
    Ash.create!(Comment, %{post_id: post.id, spam?: true})

    %{post: post}
  end

  test "preparations don't see accessing_from when reading directly" do
    assert [_, _] = Ash.read!(Comment)
  end

  test "preparations see accessing_from when loading a relationship", %{post: post} do
    assert %{comments: [%{spam?: false}]} = Ash.load!(post, :comments)
  end
end
