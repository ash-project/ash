# Testing

Take a look at [the how-to guide](/documentation/how-to/test-resources.livemd) for a practical look at writing tests

The configuration you likely want to add to your `config/test.exs` is:

```elixir
# config/test.exs
config :ash, :disable_async?, true
config :ash, :missed_notifications, :ignore
```

Each option is explained in more detail below.

## Async tests

The first thing you will likely want to do, especially if you are using `AshPostgres`, is to add the following config to your `config/test.exs`.

```elixir
# config/test.exs
config :ash, :disable_async?, true
```

This ensures that Ash does not spawn tasks when executing your requests, which is necessary for doing transactional tests with `AshPostgres`.

## Missed notifications

If you are using Ecto's transactional features to ensure that your tests all run in a transaction, Ash will detect that it had notifications to send (if you have any notifiers set up) but couldn't because it was still in a transaction. The default behavior when notifications are missed is to warn. However, this can get pretty noisy in tests. So we suggest adding the following config to your `config/test.exs`.

```elixir
# config/test.exs
config :ash, :missed_notifications, :ignore
```
