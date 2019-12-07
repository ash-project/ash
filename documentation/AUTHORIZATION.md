The defintion of authorization is "the function of specifying access rights/privileges to resources". Many authorization libraries comingle authorization (which ultimately is boolean yes or no result) along with the shape and scope of data returned depending on the result of an authorization check.

Here's a small example. Admin users can see the captured IP address of that users had when the signed up, but regular users can't. If we were to build an authorization scheme with this in mind we would show/hide that field depending on the user that was passed in. This would mean that the result of the same exact query would be different depending on the user:

```
Ash.get(User, 1, some_non-admin_user) #does NOT include the IP address
Ash.get(User, 1, some_admin_user) #includes the IP address
```

This is bad becasue ______

Ash tries to encourage good architecture and separation of concerns, and thus does not support features such as hiding fields based off the user.

*The only outcome that an authorization check should result in is either a {ok: "success" } tuple or a {error: "forbidden"} tuple. Thats it. Ash does not and never will change the underlying data of a response based on the user provided to an action.*

So what to do about changing the shape of the data depending on who is requesting it or in what context they are requesting it?

There are two options.

*TBD*
First for truly one-off situations, write custom actions that configure fields to be hidden, and call those actions.
*/TBD*

Second, and most importantly, start thinking in [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design). This is a critical concept ushered into Phoenix with Contexts in 1.3 and is the key to making easy to understand authoriation code. However, this will require a slight learning curve for folks who haven't seen this before - but it's pretty easy to grasp. Let's take an example of a small blog publishing app with Users, Posts, and Comments database tables.

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

If you try to load a post and sideload comments `Ash.get(Post, 1, side_load: [:user, :comments, :user.comments], user_requesting_this_data)` which would coorespond to a web request from `/api/posts/1?include=user,comments,comments.user` - this should not return the emails of the users that made comments (although it _should_ return the email of the user who is the author).

As you can imagine, making this authorization rule gets...complicated. Do you inspect the URL of the request? Do you look at the current user and just return the email for the user that is logged in and hide the emails of the other users?

Instead of polluting your resources with loads of conditional rules that breach the web and data layers - instead opt for a DDD approach.

If we were to think of the differnet domain models - we could come up with something like this:
```
Blog Context
  Author
  Post
  Comment
  Reader

Account Context
  User

Billing Context
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

This would allow us have clients make more specific web requests such as to `/api/blog_posts/1?include=blog_author,blog_comments,blog_comments.blog_commenter` which underlying would use `Ash.get(BlogPost, 1, side_load: [:blog_author, :blog_comments, :blog_comments,commenter], user_requesting_this_data)` to get the data we want. As you can see, much of the logic to hide/show fields has gone into what a _resource_ is by using DDD.

There will still be authorization rules, but by first thinking through architecture, we can limit and simplify them.

Go on to talk about rules around Create, Update, Read, Destroy, etc...
