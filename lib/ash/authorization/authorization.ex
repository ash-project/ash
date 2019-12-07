defmodule Ash.Authorization do
  @moduledoc """
  The key to making easy to understand authoriation code requires you first to think in [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design). Let's take an example of a small blog publishing app with Users, Posts, and Comments database tables.

  ```
  Users Table
    first_name
    last_name
    email
    password
    membership_tier
    stripe_id
    birthday
    role

  Posts Table
    user_id
    text

  Comments Table
    user_id
    post_id
    text
  ```

  In this small app, Users can be either authors or readers and will have different CRUD permissions based on that role.

  The simplist approach, which most apps do, is to just map your database tables 1-to-1 to resources.

  ```
  defmodule User do
    use Ash.Resource, name: "users", type: "user"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)
      attribute(:email, :string)
      attribute(:password, :string)
      attribute(:membership_tier, :string)
      attribute(:stripe_id, :string)
      attribute(:birthday, :date)
      attribute(:role, :string)
    end

    relationships do
      has_many(:posts, Post)
    end
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:text, :string)
    end

    relationships do
      belongs_to(:user, User)
      has_many(:posts, Post)
      has_many(:comments, Comment)
    end
  end

  defmodule Comment do
    use Ash.Resource, name: "comments", type: "comment"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:text, :string)
    end

    relationships do
      belongs_to(:user, User)
      belongs_to(:post, Post)
    end
  end
  ```

  This will absolutely work with Ash, but as you will see, it becomes problematic with even one simple authorization requirement.

  Here's an example requirement that seems simple but is non-trivial: _a commenter's email should not be fetchable when loading comments on a post._

  If you try to load a post and sideload comments `/api/posts/1?include=user,comments,comments.user` - this should not return the emails of the users that made comments (although it _should_ return the email of the user who is the author).

  As you can imagine, making this authorization rule gets...complicated. Do you inspect the URL of the request? Do you look at the current user and just return the email for the user that is logged in and hide the emails of the other users?

  Instead of polluting your resources with loads of conditional rules that breach the web and data layers - instead opt for a DDD approach.

  If we were to think of the differnet domain models - we could come up with something like this:
  ```
  Blog
    Author
    Post
    Comment
    Reader

  Account
    User

  Billing
    User
  ```

  In turn, we would create the corresponding resources
  ```
  defmodule BlogAuthor do
    use Ash.Resource, name: "users", type: "user"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)
      attribute(:email, :string)
    end

    relationships do
      has_many(:posts, Post)
    end
  end

  defmodule BlogPost do
    use Ash.Resource, name: "posts", type: "post"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:text, :string)
    end

    relationships do
      belongs_to(:user, User)
      has_many(:posts, Post)
      has_many(:comments, Comment)
    end
  end

  defmodule BlogComment do
    use Ash.Resource, name: "comments", type: "comment"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:text, :string)
    end

    relationships do
      belongs_to(:user, User)
      belongs_to(:post, Post)
    end
  end

  defmodule BlogCommenter do
    use Ash.Resource, name: "users", type: "user"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)
    end

    relationships do
      has_many(:posts, Post)
    end
  end

  defmodule BillingUser do
    use Ash.Resource, name: "users", type: "user"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)
      attribute(:membership_tier, :string)
      attribute(:stripe_id, :string)
    end
  end

  defmodule AccountUser do
    use Ash.Resource, name: "users", type: "user"
    use AshJsonApi.JsonApiResource
    use Ash.DataLayer.Postgres

    attributes do
      attribute(:first_name, :string)
      attribute(:last_name, :string)
      attribute(:birthday, :date)
      attribute(:email, :string)
      attribute(:password, :string)
      attribute(:role, :string)
    end
  end
  ```

  This would allow us to make a request to `/api/blog_posts/1?include=blog_author,blog_comments,blog_comments.blog_commenter` to get the data we want. As you can see, much of the logic to hide/show fields has gone into what a _resource_ is by using DDD.

  There will still be authorization rules, but by first thinking through architecture, we can limit and simplify them.

  Go on to talk about rules around Create, Update, Read, Destroy, etc...
  """
end
