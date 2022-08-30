# Testing

## Async tests

The first thing you will likely want to do, especially if you are using `AshPostgres`, is to add the following config to your `config/test.exs`.

```elixir
# config/test.exs
config :ash, :disable_async?, true
```

This ensures that Ash does not spawn tasks when executing your requests, which is necessary for doing transactional tests with `AshPostgres`.