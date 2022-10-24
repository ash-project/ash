defmodule Ash.Test.Policy.ComplexTest do
  @doc false
  use ExUnit.Case, async?: false
  require Ash.Query

  alias Ash.Test.Support.PolicyComplex.{Api, Comment, Post, User}

  setup do
    Application.put_env(:ash, :policies, show_policy_breakdowns?: true)
    Logger.configure(level: :debug)

    on_exit(fn ->
      Application.delete_env(:ash, :policies)
    end)

    me = User.create!("me", %{email: "me@app.com"})
    my_friend = User.create!("my friend", %{email: "my_friend@app.com"})

    a_friend_of_my_friend =
      User.create!("a friend of my friend", %{email: "friends_friend@app.com"})

    User.add_friend!(me, my_friend.id, actor: me)
    User.add_friend!(my_friend, a_friend_of_my_friend.id, actor: my_friend)
    post_by_me = Post.create!("post by me", actor: me)
    post_by_my_friend = Post.create!("post by my friend", actor: my_friend)

    post_by_a_friend_of_my_friend =
      Post.create!("post by a friend of my friend", actor: a_friend_of_my_friend)

    comment_by_me_on_my_post =
      Comment.create!(post_by_me.id, "comment by me on my own post", actor: me)

    comment_by_my_friend_on_my_post =
      Comment.create!(post_by_me.id, "comment by my friend on my", actor: my_friend)

    comment_by_a_friend_of_a_friend_on_his_own_post =
      Comment.create!(
        post_by_a_friend_of_my_friend.id,
        "comment by a friend of a friend on his own post",
        actor: a_friend_of_my_friend
      )

    comment_by_a_friend_of_a_friend_on_my_friends_post =
      Comment.create!(
        post_by_my_friend.id,
        "comment by a friend of a friend on my post",
        actor: a_friend_of_my_friend,
        # bypass auth to make a comment in this state
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

  test "it properly limits on reads", %{me: me} do
    assert [_, _] =
             Post
             |> Api.read!(actor: me)
  end

  test "it properly limits on reads of comments", %{me: me} do
    assert [_, _] =
             Comment
             |> Api.read!(actor: me)
  end

  test "it properly scopes filters", %{me: me} do
    User
    |> Ash.Query.filter(posts.exists(author.friends, name == "me"))
    |> Api.read!()

    assert [_] =
             Post
             |> Ash.Query.filter(comments.text == "comment by a friend of a friend on my post")
             |> Api.read!(actor: me, authorize?: false)

    assert [] =
             Post
             |> Ash.Query.filter(comments.text == "comment by a friend of a friend on my post")
             |> Api.read!(actor: me)
  end

  test "it properly scopes single loads" do
    assert [%{best_friend: %{name: "me"}}] =
             User
             |> Ash.Query.filter(best_friend.name == "me")
             |> Api.read!()
             |> Api.load!(:best_friend)
  end

  test "aggregates can be loaded", %{me: me} do
    Post
    |> Ash.Query.load(:count_of_comments)
    |> Ash.Query.filter(count_of_comments == 10)
    |> Api.read!(actor: me)
  end

  test "data can be loaded without forbidden errors from selecting", %{me: me} do
    users =
      Ash.Test.Support.PolicyComplex.User
      |> Ash.Query.deselect(:private_email)
      |> Api.read!(actor: me)

    Application.put_env(:foo, :bar, true)

    users
    |> Api.load!([:posts], actor: me, authorize?: true)
  end
end
