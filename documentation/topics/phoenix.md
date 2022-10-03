# Phoenix

Ash plays nicely with phoenix. There are a few things to consider when using them side-by-side.

## Adding Ash to an existing Phoenix app

To add Ash to an existing application is easy, generally only involves updating your `Ecto.Repo` to use `AshPostgres.Repo` if you are using `AshPostgres`. Other than that, you can follow the guides as usual.

## Creating a new Phoenix app

If you want to use `AshPostgres`, you have two options here:

1. create a phoenix app as normal, and when you set up `AshPostgres`, ignore the steps for creating the repo, and instead update it to use `AshPostgres.Repo`.
2. create the app with `--no-ecto` and follow the AshPostgres guide getting started guide fully.

## Using Extensions

If you are using extensions like AshGraphql or AshJsonApi, you will want to follow their getting started guides separately.