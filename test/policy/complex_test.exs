# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.ComplexTest do
  @doc false
  use ExUnit.Case, async: false
  require Ash.Query

  alias Ash.Test.Support.PolicyComplex.{Bio, Comment, Post, User}

  setup do
    old_env = Application.get_env(:ash, :policies, [])

    Application.put_env(
      :ash,
      :policies,
      Keyword.merge(old_env, show_policy_breakdowns?: true)
    )

    on_exit(fn ->
      Application.put_env(:ash, :policies, old_env)
    end)

    me = User.create!("me", %{email: "me@app.com", bio_text: "this is my bio"}, authorize?: false)
    my_friend = User.create!("my friend", %{email: "my_friend@app.com"}, authorize?: false)

    a_friend_of_my_friend =
      User.create!("a friend of my friend", %{email: "friends_friend@app.com"}, authorize?: false)

    User.add_friend!(me, my_friend.id, actor: me, authorize?: false)

    User.add_friend!(my_friend, a_friend_of_my_friend.id, actor: my_friend, authorize?: false)
    post_by_me = Post.create!("post by me", actor: me, authorize?: false)
    post_by_my_friend = Post.create!("post by my friend", actor: my_friend, authorize?: false)

    post_by_a_friend_of_my_friend =
      Post.create!("post by a friend of my friend",
        actor: a_friend_of_my_friend,
        authorize?: false
      )

    comment_by_me_on_my_post =
      Comment.create!(post_by_me.id, "comment by me on my own post", actor: me, authorize?: false)

    comment_by_my_friend_on_my_post =
      Comment.create!(post_by_me.id, "comment by my friend on my",
        actor: my_friend,
        authorize?: false
      )

    # comment_by_a_friend_of_a_friend_on_my_post =
    Comment.create!(
      post_by_me.id,
      "comment by a friend of a friend on my post",
      actor: a_friend_of_my_friend,
      authorize?: false
    )

    comment_by_a_friend_of_a_friend_on_his_own_post =
      Comment.create!(
        post_by_a_friend_of_my_friend.id,
        "comment by a friend of a friend on his own post",
        actor: a_friend_of_my_friend,
        authorize?: false
      )

    comment_by_a_friend_of_a_friend_on_my_friends_post =
      Comment.create!(
        post_by_my_friend.id,
        "comment by a friend of a friend on my friend's post",
        actor: a_friend_of_my_friend,
        authorize?: false
      )

    [
      me: me,
      my_friend: my_friend,
      a_friend_of_my_friend: a_friend_of_my_friend,
      post_by_me: post_by_me,
      post_by_my_friend: post_by_my_friend,
      post_by_a_friend_of_my_friend: post_by_a_friend_of_my_friend,
      comment_by_me_on_my_post: comment_by_me_on_my_post,
      comment_by_my_friend_on_my_post: comment_by_my_friend_on_my_post,
      comment_by_a_friend_of_a_friend_on_his_own_post:
        comment_by_a_friend_of_a_friend_on_his_own_post,
      comment_by_a_friend_of_a_friend_on_my_friends_post:
        comment_by_a_friend_of_a_friend_on_my_friends_post
    ]
  end

  test "it properly limits on reads", %{
    me: me,
    post_by_me: post_by_me,
    post_by_my_friend: post_by_my_friend
  } do
    assert [post_by_me.id, post_by_my_friend.id] |> Enum.sort() ==
             Post
             |> Ash.read!(actor: me)
             |> Enum.map(& &1.id)
             |> Enum.sort()
  end

  test "it applies policies from the domain", %{me: me} do
    assert_raise Ash.Error.Forbidden,
                 ~r/authorize unless: actor.forbidden_by_domain == true | ✓ |/,
                 fn ->
                   Ash.read!(Post, actor: %{me | forbidden_by_domain: true})
                 end
  end

  test "it properly limits on reads of comments", %{
    me: me,
    comment_by_me_on_my_post: comment_by_me_on_my_post,
    comment_by_my_friend_on_my_post: comment_by_my_friend_on_my_post
  } do
    assert [comment_by_me_on_my_post.id, comment_by_my_friend_on_my_post.id] |> Enum.sort() ==
             Comment
             |> Ash.read!(actor: me)
             |> Enum.map(& &1.id)
             |> Enum.sort()
  end

  test "it properly scopes filters", %{me: me} do
    assert [_] =
             Post
             |> Ash.Query.filter(comments.text == "comment by a friend of a friend on my post")
             |> Ash.read!(actor: me, authorize?: false)

    assert [] =
             Post
             |> Ash.Query.filter_input(
               comments: [text: "comment by a friend of a friend on my post"]
             )
             |> Ash.read!(actor: me)
  end

  test "it properly scopes single loads", %{me: me} do
    assert [%{best_friend: %{name: "me"}}] =
             User
             |> Ash.Query.filter(best_friend.name == "me")
             |> Ash.Query.deselect(:private_email)
             |> Ash.read!(actor: me)
  end

  test "aggregates can be loaded and filtered on", %{me: me} do
    Post
    |> Ash.Query.load(:count_of_comments)
    |> Ash.Query.filter(count_of_comments == 10)
    |> Ash.read!(actor: me)
  end

  test "forbidden fields can be returned for relationships", %{me: me} do
    assert [%Ash.ForbiddenField{}, %Ash.ForbiddenField{}] =
             Post
             |> Ash.Query.load(:forbidden_field_author)
             |> Ash.read!(actor: me)
             |> Enum.map(& &1.forbidden_field_author)
  end

  test "calculations containing aggregates authorize their aggregates", %{
    me: me,
    post_by_my_friend: post_by_my_friend,
    a_friend_of_my_friend: a_friend_of_my_friend
  } do
    Comment.create!(
      post_by_my_friend.id,
      "comment by a friend of a friend on my friend's post",
      actor: a_friend_of_my_friend,
      authorize?: false
    )

    Comment.create!(
      post_by_my_friend.id,
      "comment by a friend of a friend on my friend's post",
      actor: a_friend_of_my_friend,
      authorize?: false
    )

    Comment.create!(
      post_by_my_friend.id,
      "comment by a friend of a friend on my friend's post",
      actor: a_friend_of_my_friend,
      authorize?: false
    )

    assert [2, 0] ==
             Post
             |> Ash.Query.load(:count_of_comments_calc)
             |> Ash.Query.sort(count_of_comments_calc: :desc)
             |> Ash.read!(actor: me)
             |> Enum.map(& &1.count_of_comments_calc)
  end

  test "aggregates join paths are authorized", %{me: me, post_by_me: post_by_me} do
    count_of_commenters_without_authorization =
      Post
      |> Ash.Query.load(:count_of_commenters)
      |> Ash.Query.filter(id == ^post_by_me.id)
      |> Ash.read_one!(authorize?: false)
      |> Map.get(:count_of_commenters)

    assert count_of_commenters_without_authorization == 3

    count_of_commenters_with_authorization =
      Post
      |> Ash.Query.load(:count_of_commenters)
      |> Ash.Query.filter(id == ^post_by_me.id)
      |> Ash.read_one!(actor: me)
      |> Map.get(:count_of_commenters)

    assert count_of_commenters_with_authorization == 2
  end

  test "multiple aggregates join paths are authorized", %{me: me, post_by_me: post_by_me} do
    assert %{always_forbidden_comments: 0, always_forbidden_author: 0} =
             Post
             |> Ash.Query.load([:always_forbidden_comments, :always_forbidden_author])
             |> Ash.Query.filter(id == ^post_by_me.id)
             |> Ash.read_one!(actor: me)
             |> Map.take([:always_forbidden_comments, :always_forbidden_author])
  end

  test "aggregates in calculations are authorized", %{me: me} do
    Post
    |> Ash.Query.load([:count_of_comments_calc, :count_of_comments])
    |> Ash.read!(actor: me, authorize?: true)
  end

  test "data can be loaded without forbidden errors from selecting", %{me: me} do
    users =
      Ash.Test.Support.PolicyComplex.User
      |> Ash.Query.deselect(:private_email)
      |> Ash.read!(actor: me)

    users
    |> Ash.load!([:posts], actor: me, authorize?: true)
  end

  test "loading data honors `accessing_from` policies", %{me: me} do
    Ash.load!(me, [:bio], authorize?: true, actor: me)
    me |> User.set_bio!("New bio!", authorize?: true, actor: me)

    User
    |> Ash.Query.filter(bio == "New bio!")
    |> Ash.Query.deselect(:private_email)
    |> Ash.read_one!(authorize?: true, actor: me)

    me |> Ash.load!([:bio_text], authorize?: true, actor: me)

    assert [] = Ash.read!(Bio, actor: me, authorize?: true)
  end

  test "it calls runtime checks when policy access_type == :runtime", %{
    me: me,
    comment_by_me_on_my_post: comment_by_me_on_my_post
  } do
    comment_id = comment_by_me_on_my_post.id

    assert [_] =
             Comment
             |> Ash.Query.for_read(:read_with_runtime_check, %{}, actor: me)
             |> Ash.Query.filter(id == ^comment_id)
             |> Ash.read!()

    assert_received {:runtime_check_executed, [_]}
  end

  test "it calls runtime checks when policy access_type == :runtime, removing records that don't match",
       %{
         me: me,
         comment_by_me_on_my_post: comment_by_me_on_my_post
       } do
    comment_id = comment_by_me_on_my_post.id

    Process.put(:fail_runtime_check, true)

    assert [] =
             Comment
             |> Ash.Query.for_read(:read_with_runtime_check, %{}, actor: me)
             |> Ash.Query.filter(id == ^comment_id)
             |> Ash.read!()

    assert_received {:runtime_check_executed, [_]}
  end

  test "calculations get context and actor", %{me: me, post_by_me: post} do
    assert [_] = Post.erasable!(actor: me, context: %{post_id: post.id})
    assert %{text: "[deleted]"} = Post.erase!(post, actor: me, context: %{post_id: post.id})
  end
end
