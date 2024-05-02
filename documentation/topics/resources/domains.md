# Domains

Domains serve three primary purposes:

1. They group related resources together, providing organization and structure to your project.
2. They allow you to define a centralized [code interface](/documentation/topics/resources/code-interfaces.md)
3. They allow you to configure certain cross-cutting concerns of those resources in a single place.

If you are familiar with a [Phoenix Context](https://hexdocs.pm/phoenix/contexts.html), you can think of a domain as the Ash equivalent.

## Grouping Resources

In an `Ash.Domain`, you will typically see something like this:

```elixir
defmodule MyApp.Tweets do
  use Ash.Domain

  resources do
    resource MyApp.Tweets.Tweet
    resource MyApp.Tweets.Comment
  end
end
```

With this definition, you can do things like placing all of these resources into a GraphQL Api with AshGraphql. You'd see a line like this:

```elixir
use AshGraphql, domains: [MyApp.Tweets]
```

## Centralized [Code Interface](/documentation/topics/resources/code-interfaces.md)

Working with our domain & resources in code *can* be done the long form way, by building changesets/queries/action inputs and calling the relevant function in `Ash`. However, we generally want to expose a well defined code API for working with our resources. This makes our code much clearer, and gives us nice things like auto complete and inline documentation.

```elixir
defmodule MyApp.Tweets do
  use Ash.Domain

  resources do
    resource MyApp.Tweets.Tweet do
      # define a function called `tweet` that uses
      # the `:create` action on MyApp.Tweets.Tweet
      define :tweet, action: :create, args: [:text]
    end

    resource MyApp.Tweets.Comment do
      # define a function called `comment` that uses
      # the `:create` action on MyApp.Tweets.Comment
      define :comment, action: :create, args: [:tweet_id, :text]
    end
  end
end
```

With these definitions, we can now do things like this:

```elixir
tweet = MyApp.Tweets.tweet!("My first tweet!", actor: user1)
comment = MyApp.Tweets.comment!(tweet.id, "What a cool tweet!", actor: user2)
```

## Configuring Cross-cutting Concerns

### Built in configuration

`Ash.Domain` comes with a number of built-in configuration options. See `d:Ash.Domain` for more.

For example:

```elixir
defmodule MyApp.Tweets do
  use Ash.Domain

  resources do
    resource MyApp.Tweets.Tweet
    resource MyApp.Tweets.Comment
  end

  execution do
    # raise the default timeout for all actions in this domain from 30s to 60s
    timeout :timer.seconds(60)
  end

  authorization do
    # disable using the authorize?: false flag when calling actions
    authorize :always
  end
end
```

### Extensions

Extensions will often come with "domain extensions" to allow you to configure the behavior of all resources within a domain, as it pertains to that extension. For example:

```elixir
defmodule MyApp.Tweets do
  use Ash.Domain,
    extensions: [AshGraphql.Domain]

  graphql do
    # skip authorization for these resources
    authorize? false
  end

  resources do
    resource MyApp.Tweets.Tweet
    resource MyApp.Tweets.Comment
  end
end
```

### Policies

You can also use `Ash.Policy.Authorizer` on your domains. This allows you to add policies that apply to *all* actions using this domain. For example:

```elixir
defmodule MyApp.Tweets do
  use Ash.Domain,
    extensions: [Ash.Policy.Authorizer]

  resources do
    resource MyApp.Tweets.Tweet
    resource MyApp.Tweets.Comment
  end

  policies do
    # add a bypass up front to allow administrators to do whatever they want
    bypass actor_attribute_equals(:is_admin, true) do
      authorize_if always()
    end

    # forbid all access from disabled users
    policy actor_attribute_equals(:disabled, true) do
      forbid_if always()
    end
  end
end
```
