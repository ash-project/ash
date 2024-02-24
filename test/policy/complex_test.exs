defmodule Ash.Test.Policy.ComplexTest do
  @doc false
  use ExUnit.Case, async?: false
  require Ash.Query

  alias Ash.Test.Support.PolicyComplex.{Bio, Comment, Domain, Post, User}

  setup do
    Application.put_env(:ash, :policies, show_policy_breakdowns?: true)

    on_exit(fn ->
      Application.delete_env(:ash, :policies)
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
      post_by_me: post_by_me,
      post_by_my_friend: post_by_my_friend,
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
             |> Domain.read!(actor: me)
             |> Enum.map(& &1.id)
             |> Enum.sort()
  end

  test "it properly limits on reads of comments", %{
    me: me,
    comment_by_me_on_my_post: comment_by_me_on_my_post,
    comment_by_my_friend_on_my_post: comment_by_my_friend_on_my_post
  } do
    assert [comment_by_me_on_my_post.id, comment_by_my_friend_on_my_post.id] |> Enum.sort() ==
             Comment
             |> Domain.read!(actor: me)
             |> Enum.map(& &1.id)
             |> Enum.sort()
  end

  test "it properly scopes filters", %{me: me} do
    assert [_] =
             Post
             |> Ash.Query.filter(comments.text == "comment by a friend of a friend on my post")
             |> Domain.read!(actor: me, authorize?: false)

    assert [] =
             Post
             |> Ash.Query.filter_input(
               comments: [text: "comment by a friend of a friend on my post"]
             )
             |> Domain.read!(actor: me)
  end

  test "it properly scopes single loads", %{me: me} do
    assert [%{best_friend: %{name: "me"}}] =
             User
             |> Ash.Query.filter(best_friend.name == "me")
             |> Ash.Query.deselect(:private_email)
             |> Domain.read!(actor: me)
             |> Domain.load!(:best_friend, actor: me)
  end

  test "aggregates can be loaded and filtered on", %{me: me} do
    Post
    |> Ash.Query.load(:count_of_comments)
    |> Ash.Query.filter(count_of_comments == 10)
    |> Domain.read!(actor: me)
  end

  test "aggregates join paths are authorized", %{me: me, post_by_me: post_by_me} do
    count_of_commenters_without_authorization =
      Post
      |> Ash.Query.load(:count_of_commenters)
      |> Ash.Query.filter(id == ^post_by_me.id)
      |> Domain.read_one!(authorize?: false)
      |> Map.get(:count_of_commenters)

    assert count_of_commenters_without_authorization == 3

    count_of_commenters_with_authorization =
      Post
      |> Ash.Query.load(:count_of_commenters)
      |> Ash.Query.filter(id == ^post_by_me.id)
      |> Domain.read_one!(actor: me)
      |> Map.get(:count_of_commenters)

    assert count_of_commenters_with_authorization == 2
  end

  test "aggregates in calculations are authorized", %{me: me} do
    Post
    |> Ash.Query.load([:count_of_comments_calc, :count_of_comments])
    |> Domain.read!(actor: me, authorize?: true)
  end

  test "data can be loaded without forbidden errors from selecting", %{me: me} do
    users =
      Ash.Test.Support.PolicyComplex.User
      |> Ash.Query.deselect(:private_email)
      |> Domain.read!(actor: me)

    users
    |> Domain.load!([:posts], actor: me, authorize?: true)
  end

  test "loading data honors `accessing_from` policies", %{me: me} do
    Domain.load!(me, [:bio], authorize?: true, actor: me)
    me |> User.set_bio!("New bio!", authorize?: true, actor: me)

    User
    |> Ash.Query.filter(bio == "New bio!")
    |> Ash.Query.deselect(:private_email)
    |> Domain.read_one!(authorize?: true, actor: me)

    me |> Domain.load!([:bio_text], authorize?: true, actor: me)

    assert_raise Ash.Error.Forbidden, fn ->
      Domain.read!(Bio, actor: me, authorize?: true)
    end
  end
end
