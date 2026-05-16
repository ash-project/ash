<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

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

## DataCase usage

When using `AshPostgres`, you should use `DataCase` instead of `ExUnit.Case`. Without DataCase, the database connection will fail, resulting in `DBConnection.OwnershipError`, and most of the doctests will break for the same reason. 

```elixir
# test/my_app/accounts_test.exs
defmodule MyApp.AccountsTest do
  use MyApp.DataCase

  test "creates a user" do
    # ...
  end
end
```
DataCase is a reusable setup module that automatically imports everything necessary from the Ecto library, so you don't have to write alias and import every test file. Before every test, it checks out the database connection, wrapping everything that the test does inside of a database transaction and rolling it back after the test ends, regardless of its result. Checking the connection is what helps us avoid the `DBConnection.OwnershipError`.

For a practical look at writing tests, see [the how-to guide](/documentation/how-to/test-resources.livemd).

For a further explanation on how the sandbox works:
See [Ash-Postgres guide](https://hexdocs.pm/ash_postgres/readme.html)

## Missed notifications

If you are using Ecto's transactional features to ensure that your tests all run in a transaction, Ash will detect that it had notifications to send (if you have any notifiers set up) but couldn't because it was still in a transaction. The default behavior when notifications are missed is to warn. However, this can get pretty noisy in tests. So we suggest adding the following config to your `config/test.exs`.

```elixir
# config/test.exs
config :ash, :missed_notifications, :ignore
```
