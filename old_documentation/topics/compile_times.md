# Improving Compile Times

In previous versions of Ash, the standard way to configure the list of resources for an Api module looked like this:

```elixir
defmodule MyApp.MyApi do
  use Ash.Api


  resources do
    resource MyApp.MyResource
    ...
  end
end
```

This caused many compilation dependency issues, causing slow compile times when changing single files, and could also potentially lead to deadlocks.

The preferred way of doing this now looks like this:

```elixir
# Define a registry module
defmodule MyApp.MyApi.Registry do
  use Ash.Registry,
    extensions: Ash.Registry.ResourceValidations

  entries do
    entry MyApp.MyResource
    ...
  end
end

defmodule MyApp.MyApi do
  use Ash.Api, otp_app: :my_app
end

# in `config/config.exs`

config :my_app, MyApp.MyApi,
  resources: [
    registry: MyApp.MyApi.Registry
  ]
```

This will prevent a bunch of cross-concern compile time dependencies, allowing for much faster compile times in general.
