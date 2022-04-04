# Authorization

## Ash Policy Authorizer

Generally speaking, you will want to use ash_policy_authorizer to authorize access to your resources.

use `mix hex.info ash_policy_authorizer` to get the latest version, and add it to your dependencies:

```elixir
{:ash_policy_authorizer, "~> x.x.x"}
```

For usage, see the `ash_policy_authorizer` [documentation](https://hexdocs.pm/ash_policy_authorizer) for the rest

## Implementing a custom authorizer

Implementing a custom authorizer is pretty complex. Instead of writing a guide, it would be best to just have some discussions if/when someone thinks that they need one. Make an issue and we'll talk it over.
