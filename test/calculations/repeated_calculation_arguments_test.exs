# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.RepeatedCalculationArgumentsTest do
  @moduledoc """
  Expression calculations are inlined, and a calculation referenced from several places (directly
  or through other calculations) is expanded once and shared by every reference. These check that
  each reference keeps its own meaning: its arguments, its relationship path, the aggregates it
  uses, `parent/1` scoping, and authorization.
  """
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Item do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Calculations.RepeatedCalculationArgumentsTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :base, :integer, public?: true
    end

    calculations do
      calculate :plus, :integer, expr(base + ^arg(:amount)) do
        public? true
        argument :amount, :integer, allow_nil?: false
      end

      calculate :combined, :integer, expr(plus(amount: 1) * 1000 + plus(amount: 2)), public?: true

      calculate :nested_combined, :integer, expr(combined * 10 + plus(amount: 3)), public?: true
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Calculations.RepeatedCalculationArgumentsTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :likes, :integer, public?: true
      attribute :visible, :boolean, public?: true, default: true
    end

    relationships do
      belongs_to :post, Ash.Test.Calculations.RepeatedCalculationArgumentsTest.Post, public?: true
    end

    calculations do
      # shares its name with `Post.base`
      calculate :base, :integer, expr(likes * 2)
      calculate :base_plus_post_base, :integer, expr(base + (post.base || 0) * 1000)
    end

    policies do
      policy action_type(:read) do
        authorize_if expr(visible == true)
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Calculations.RepeatedCalculationArgumentsTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :score, :integer, public?: true
    end

    relationships do
      belongs_to :parent_post, __MODULE__, public?: true
      has_many :comments, Comment, public?: true
    end

    aggregates do
      count :count_of_comments, :comments
    end

    calculations do
      calculate :base, :integer, expr((score || 0) + count_of_comments)
      calculate :mid, :integer, expr(base * 2 + base)
      calculate :top, :integer, expr(if(mid > 10, do: mid - base + mid, else: mid + base))

      calculate :with_parent_post, :integer, expr(top + (parent_post.top || 0) * 100_000 + top)

      calculate :comment_base_above_base,
                :boolean,
                expr(exists(comments, base > parent(base)) and base > 0)

      calculate :base_above_parent_post,
                :boolean,
                expr(exists(parent_post, base < parent(base)))

      calculate :authorized_has_comments, :boolean, expr(exists(comments, true)) do
        authorize? true
      end

      calculate :authorized_has_comments_chain,
                :string,
                expr(
                  if(authorized_has_comments, do: "a", else: "b") <>
                    if(authorized_has_comments and base >= 0, do: "c", else: "d")
                )
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  defp expected(posts) do
    comments = Comment |> Ash.read!(authorize?: false) |> Enum.group_by(& &1.post_id)

    base =
      Map.new(posts, fn post ->
        {post.id, (post.score || 0) + length(Map.get(comments, post.id, []))}
      end)

    top =
      Map.new(base, fn {id, base} ->
        mid = base * 3
        {id, if(mid > 10, do: mid - base + mid, else: mid + base)}
      end)

    Map.new(posts, fn post ->
      base = base[post.id]
      post_comments = Map.get(comments, post.id, [])

      {post.id,
       %{
         base: base,
         mid: base * 3,
         top: top[post.id],
         with_parent_post:
           top[post.id] + ((post.parent_post_id && top[post.parent_post_id]) || 0) * 100_000 +
             top[post.id],
         comment_base_above_base: Enum.any?(post_comments, &(&1.likes * 2 > base)) and base > 0,
         base_above_parent_post:
           not is_nil(post.parent_post_id) and
             Map.fetch!(base_of(posts, post.parent_post_id, comments), :base) < base,
         visible_comments: Enum.any?(post_comments, & &1.visible),
         comments: post_comments != []
       }}
    end)
  end

  defp base_of(posts, id, comments) do
    post = Enum.find(posts, &(&1.id == id))
    %{base: (post.score || 0) + length(Map.get(comments, id, []))}
  end

  @calcs [
    :base,
    :mid,
    :top,
    :with_parent_post,
    :comment_base_above_base,
    :base_above_parent_post
  ]

  describe "references with different arguments" do
    setup do
      %{item: Ash.create!(Item, %{base: 5})}
    end

    test "each reference to a calculation uses its own arguments when loaded", %{item: item} do
      assert %{combined: 6007, nested_combined: 60_078} =
               Ash.load!(item, [:combined, :nested_combined])
    end

    test "each reference to a calculation uses its own arguments when read", %{item: item} do
      assert [%{combined: 6007}] =
               Item
               |> Ash.Query.filter(id == ^item.id)
               |> Ash.Query.load(:combined)
               |> Ash.read!()
    end

    test "each reference to a calculation uses its own arguments in a filter", %{item: item} do
      assert [_] = Item |> Ash.Query.filter(combined == 6007) |> Ash.read!()
      assert [] = Item |> Ash.Query.filter(combined in [6006, 7007]) |> Ash.read!()
      assert [_] = Item |> Ash.Query.filter(plus(amount: 1) < plus(amount: 2)) |> Ash.read!()
      _ = item
    end

    test "each reference to a calculation uses its own arguments with Ash.calculate", %{
      item: item
    } do
      assert {:ok, 6007} = Ash.calculate(item, :combined)
    end

    test "a calculation loaded with other arguments is not used for a reference", %{item: item} do
      assert %{combined: 6007, plus: 14} =
               Ash.load!(item, [:combined, plus: %{amount: 9}])

      assert %{combined: 6007} =
               item
               |> Ash.load!(plus: %{amount: 9})
               |> Ash.load!(:combined)
    end

    test "a runtime filter on already loaded records uses each reference's arguments", %{
      item: item
    } do
      item = Ash.load!(item, plus: %{amount: 9})

      assert {:ok, [_]} =
               Ash.Filter.Runtime.filter_matches(
                 Domain,
                 [item],
                 Ash.Filter.parse!(Item, expr(combined == 6007))
               )
    end
  end

  describe "references shared across calculations" do
    setup do
      p1 = Ash.create!(Post, %{title: "p1", score: 5})
      p2 = Ash.create!(Post, %{title: "p2", score: 3})
      p3 = Ash.create!(Post, %{title: "p3", parent_post_id: p2.id})
      p4 = Ash.create!(Post, %{title: "p4", score: 50, parent_post_id: p1.id})

      for {post, likes, visible} <- [
            {p2, 2, true},
            {p2, 10, false},
            {p3, 0, false},
            {p4, 100, true}
          ] do
        Ash.create!(Comment, %{post_id: post.id, likes: likes, visible: visible})
      end

      %{posts: [p1, p2, p3, p4]}
    end

    test "repeated calculations have the same values loaded together or alone", %{posts: posts} do
      expected = expected(posts)

      for post <- Post |> Ash.Query.load(@calcs) |> Ash.read!(authorize?: false),
          calc <- @calcs do
        assert Map.fetch!(post, calc) == expected[post.id][calc],
               "#{calc} of #{post.title}: expected #{inspect(expected[post.id][calc])}, got #{inspect(Map.fetch!(post, calc))}"
      end

      for calc <- @calcs, post <- Post |> Ash.Query.load(calc) |> Ash.read!(authorize?: false) do
        assert Map.fetch!(post, calc) == expected[post.id][calc]
      end

      assert Enum.any?(posts, &expected[&1.id].comment_base_above_base)
      assert Enum.any?(posts, &expected[&1.id].base_above_parent_post)
    end

    test "calculations of the same name on different resources are kept apart", %{posts: posts} do
      expected = expected(posts)

      comments =
        Comment
        |> Ash.Query.load(:base_plus_post_base)
        |> Ash.read!(authorize?: false)

      assert length(comments) == 4

      for comment <- comments do
        assert comment.base_plus_post_base ==
                 comment.likes * 2 + expected[comment.post_id].base * 1000
      end
    end

    test "filtering and sorting on repeated calculations", %{posts: posts} do
      expected = expected(posts)

      expected_ids =
        posts
        |> Enum.filter(&(expected[&1.id].top > 20 and expected[&1.id].with_parent_post > 0))
        |> Enum.sort_by(&{-expected[&1.id].top, &1.id})
        |> Enum.map(& &1.id)

      assert expected_ids != []

      assert expected_ids ==
               Post
               |> Ash.Query.filter(top > 20 and with_parent_post > 0)
               |> Ash.Query.sort(top: :desc, id: :asc)
               |> Ash.read!(authorize?: false)
               |> Enum.map(& &1.id)
    end

    test "a repeated authorized calculation is authorized at every reference", %{posts: posts} do
      expected = expected(posts)

      for post <-
            Post |> Ash.Query.load(:authorized_has_comments_chain) |> Ash.read!(authorize?: true) do
        assert post.authorized_has_comments_chain ==
                 if(expected[post.id].visible_comments, do: "ac", else: "bd")
      end

      for post <-
            Post |> Ash.Query.load(:authorized_has_comments_chain) |> Ash.read!(authorize?: false) do
        assert post.authorized_has_comments_chain ==
                 if(expected[post.id].comments, do: "ac", else: "bd")
      end

      assert Enum.any?(
               posts,
               &(expected[&1.id].comments and not expected[&1.id].visible_comments)
             )
    end
  end
end
