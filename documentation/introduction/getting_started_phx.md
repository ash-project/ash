# Getting started with Ash and Phoenix

In this guide we will convert the sample app from the [getting
stated guide](https://github.com/ash-project/ash/blob/master/documentation/introduction/getting_started.md) into
a full blown service backed by PostgresSQL as a storage
and a Json Web API.

For the web part of the application we will rely on the
[Phoenix framework](https://www.phoenixframework.org/) as both frameworks are complementary.
Keep in mind that Phoenix is not a requirement you could
use [Plug](https://github.com/elixir-plug/plug)

You can check the completed application in this [repo](https://github.com/mario-mazo/my_app_phx)

## Create phoenix app

We create a simple phoenix application and we remove some unnecesary parts,
also we are using `--app` to rename the application so it maches the name from
the getting started guide

```shell
mix phx.new my_app_phx --app my_app --no-html --no-webpack --no-gettext
```

## Add dependencies and formatter

Now we need to add the dependencies, `ash` and [ash_postgres](https://hexdocs.pm/ash_postgres/readme.html). To find out
what is the latest available verion you can use `mix hex.info`

```shell
mix hex.info ash_postgres
mix hex.info ash
```

then modify the the files `.formatter` and `mix.exs`

```diff
--- a/.formatter.exs
+++ b/.formatter.exs
@@ -1,4 +1,13 @@
 [
+  import_deps: [
+    :ecto,
+    :phoenix,
+    :ash,
+    :ash_json_api,
+    :ash_graphql,
+    :ash_policy_authorizer,
+    :ash_postgres
+  ],

--- b/mix.exs
+++ b/mix.exs
@@ -33,6 +33,8 @@ defmodule MyAppPhx.MixProject do
   # Type `mix help deps` for examples and options.
   defp deps do
     [
+      {:ash_postgres, "~> 0.25.5"},
+      {:ash, "~> 1.24"},

```

Make sure you can connect to postgres verify in the file `config/dev.exs`
that credentials are correct and create the database by running:

Also since Ash uses [jsonapi](https://jsonapi.org/) for the api spec it is necesary to add
the correspoinding mime type in `config/config.exs`

```diff
--- a/config/config.exs
+++ b/config/config.exs
@@ -10,6 +10,10 @@ use Mix.Config
 config :my_app,
   ecto_repos: [MyApp.Repo]

+config :mime, :types, %{
+  "application/vnd.api+json" => ["json"]
+}
+
```

```shell
mix ecto.create
* The database for MyApp.Repo has been created
```

### Reuse the files from the getting started guide

Copy the `lib/my_app/api.ex`, `lib/my_app/resources/tweet.ex`
and `lib/my_app/resources/user.ex` from the getting started
sample app into this project in the same path.

## Switch data layer to postgres

We can now proceed to switch the data layer from `ETS`
to `PostgreSQL` simply by changing the `data_layer` to
`AshPostgres.DataLayer` in our resources
and adding the table name and the Ecto
repo. In this case we will use the default repo created by phoenix.

```diff
--- a/my_app_phx/lib/my_app/resources/tweet.ex
+++ b/my_app_phx/lib/my_app/resources/tweet.ex
@@ -1,6 +1,11 @@
 # in my_app_phx/lib/my_app/resources/tweet.ex
 defmodule MyApp.Tweet do
-  use Ash.Resource, data_layer: Ash.DataLayer.Ets
+  use Ash.Resource, data_layer: AshPostgres.DataLayer
+
+  postgres do
+    table "tweets"
+    repo MyApp.Repo
+  end


--- a/my_app_phx/lib/my_app/resources/user.ex
+++ b/my_app_phx/lib/my_app/resources/user.ex
@@ -1,6 +1,11 @@
 # in my_app_phx/lib/my_app/resources/user.ex
 defmodule MyApp.User do
-  use Ash.Resource, data_layer: Ash.DataLayer.Ets
+ use Ash.Resource, data_layer: AshPostgres.DataLayer
+
+  postgres do
+    table "users"
+    repo MyApp.Repo
+  end
```

Now you can tell ash to generate the migrations from you API

```shell
mix ash_postgres.generate_migrations --apis MyApp.Api
* creating priv/repo/migrations/20201120214857_migrate_resources1.exs
```

and run the ecto migration to generate the tables

```shell

run mix ecto.migrate

23:23:46.067 [info]  == Running 20201120222312 MyApp.Repo.Migrations.MigrateResources1.up/0 forward

23:23:46.070 [info]  create table users

23:23:46.076 [info]  create table tweets

23:23:46.090 [info]  == Migrated 20201120222312 in 0.0s

```

### Test PostgreSQL integration

Start iex with `iex -S mix phx.server` and lets run the same test
we ran in the initial `my_app`. You will now see that SQL statements
are being executed and data is now stored in  your PostgreSQL database.

```elixir
iex(1)> {:ok, user} = Ash.Changeset.new(MyApp.User, %{email: "ash.man@enguento.com"}) |> MyApp.Api.create()
[debug] QUERY OK db=1.2ms idle=1432.0ms
begin []
[debug] QUERY OK db=0.4ms
INSERT INTO "users" ("email","id") VALUES ($1,$2) ["ash.man@enguento.com", <<72, 22
6, 94, 187, 145, 81, 66, 25, 183, 79, 59, 199, 93, 88, 32, 243>>]
[debug] QUERY OK db=0.3ms
commit []
{:ok,
 %MyApp.User{
   __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
   __metadata__: %{},
   aggregates: %{},
   calculations: %{},
   email: "ash.man@enguento.com",
   id: "48e25ebb-9151-4219-b74f-3bc75d5820f3",
   tweets: #Ash.NotLoaded<:relationship>
 }}

iex(2)> MyApp.Tweet |> Ash.Changeset.new(%{body: "ashy slashy"}) |> Ash.Changeset.r
eplace_relationship(:user, user) |> MyApp.Api.create()
[debug] QUERY OK db=0.1ms idle=1197.5ms
begin []
[debug] QUERY OK db=2.2ms
INSERT INTO "tweets" ("body","created_at","id","public","updated_at","user_id") VAL
UES ($1,$2,$3,$4,$5,$6) ["ashy slashy", ~U[2020-11-22 21:15:33Z], <<163, 22, 225, 4
3, 217, 10, 67, 242, 152, 149, 197, 133, 253, 154, 244, 95>>, false, ~U[2020-11-22
21:15:33Z], <<72, 226, 94, 187, 145, 81, 66, 25, 183, 79, 59, 199, 93, 88, 32, 243>
>]
[debug] QUERY OK db=0.3ms
commit []
{:ok,
 %MyApp.Tweet{
   __meta__: #Ecto.Schema.Metadata<:loaded, "tweets">,
   __metadata__: %{},
   aggregates: %{},
   body: "ashy slashy",
   calculations: %{},
   created_at: ~U[2020-11-22 21:15:33Z],
   id: "a316e12b-d90a-43f2-9895-c585fd9af45f",
   public: false,
   updated_at: ~U[2020-11-22 21:15:33Z],
   user: %MyApp.User{
     __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
     __metadata__: %{},
     aggregates: %{},
     calculations: %{},
     email: "ash.man@enguento.com",
     id: "48e25ebb-9151-4219-b74f-3bc75d5820f3",
     tweets: #Ash.NotLoaded<:relationship>
   },
   user_id: "48e25ebb-9151-4219-b74f-3bc75d5820f3"
 }}
```

### Exposing the API via WEB JSON API

We need to add the extension dependency for [ash_json_api](https://hexdocs.pm/ash_json_api/readme.html)


```shell
mix hex.info ash_json_api
```

add it to your dependencies and dont forget to run `mix deps.get`

```diff
--- a/mix.exs
+++ b/mix.exs
@@ -33,6 +33,7 @@ defmodule MyApp.MixProject do
   # Type `mix help deps` for examples and options.
   defp deps do
     [
+      {:ash_json_api, "~> 0.24.1"},
       {:ash_postgres, "~> 0.25.5"},
       {:ash, "~> 1.24"},
       {:phoenix, "~> 1.5.6"},
```

With the dependencies in place the extension has to be added to your
API in `MyApp.Api`

```diff
--- a/lib/my_app/api.ex
+++ b/lib/my_app/api.ex
@@ -1,6 +1,9 @@
 # lib/my_app/api.ex
 defmodule MyApp.Api do
-  use Ash.Api
+  use Ash.Api,
+    extensions: [
+      AshJsonApi.Api
+    ]
```

We can proceed to add a route in the Phoneix router to forward request
to our Ash API. To do so we use `AshJsonApi.forward/3` as shown in
`lib/my_app_web/router.ex`

```diff
--- a/lib/my_app_web/router.ex
+++ b/lib/my_app_web/router.ex
@@ -1,12 +1,14 @@
 defmodule MyAppWeb.Router do
   use MyAppWeb, :router
-
+  require AshJsonApi
+
   pipeline :api do
     plug :accepts, ["json"]
   end

-  scope "/api", MyAppWeb do
+  scope "/api" do
     pipe_through :api
+    AshJsonApi.forward("/", MyApp.Api)
   end
```

After that all we have to do is tell the our resources that are now
going to be part of an Json API. In this guide we will only expose an
API for the `user` resource, exposing the tweet as is left as an exersice
for the reader.

We need to add an extension to our resouce and define a mapping between
the REST verbs and our internal api actions.

```diff
--- a/lib/my_app/resources/user.ex
+++ b/lib/my_app/resources/user.ex
@@ -1,7 +1,24 @@
 # in lib/my_app/resources/user.ex
 defmodule MyApp.User do
- use Ash.Resource, data_layer: AshPostgres.DataLayer
+  use Ash.Resource, data_layer: AshPostgres.DataLayer,
+    extensions: [
+      AshJsonApi.Resource
+    ]

+  json_api do
+    type "user"
+
+    routes do
+      base "/users"
+
+      get :read
+      index :read
+      post :create
+      patch :update
+      delete :destroy
+    end
+  end
+
```

### Test Web Json API

Fire up iex with `iex -S mix phx.server` and curl the api

```shell
curl -s --request GET --url 'http://localhost:4000/api/users' | jq

{
  "data": [
    {
      "attributes": {
        "email": "ash.man@enguento.com"
      },
      "id": "46b60ec8-5b0f-461d-95ab-bcc5169ff831",
      "links": {},
      "meta": {},
      "relationships": {},
      "type": "user"
    },
    {
      "attributes": {
        "email": "ash.man@enguento.com"
      },
      "id": "cd84148a-4af4-4f9f-952f-9daa28946e01",
      "links": {},
      "meta": {},
      "relationships": {},
      "type": "user"
    },
    {
      "attributes": {
        "email": "ash.man@enguento.com"
      },
      "id": "48e25ebb-9151-4219-b74f-3bc75d5820f3",
      "links": {},
      "meta": {},
      "relationships": {},
      "type": "user"
    }
  ],
  "jsonapi": {
    "version": "1.0"
  },
  "links": {
    "self": "http://localhost:4000/api/users"
  }
}

```
