# Contexts and Domains

It is suggested that you read a bit on Domain Driven Design before proceeding. If you are using phoenix or are familiar with phoenix contextxs, then this will make sense to you.

In order to support domain driven design, Ash supports defining multiple APIs, each with their own set of resources. It is possible to share a resource between APIs, but this gets untenable very quickly because any resources related to the shared resource must _both_ appear in each API.

To help solve for this you can use the `Ash.DataLayer.Delegate` data layer, which allows you to map a resource from one API(context) to another. A common example used with DDD is the "user" object. In each context, a user may more rightly be called something else. Perhaps there is a portion of your system used for administration, in which context the user will always be an "administrator". This "administrator" has different fields available,
and different relationships. To represent this in Ash, you may have an "administration api".

When following this approach, it is advised to use the same file structure that phoenix uses, with the addition of a `resources` folder. For this example, we have

- lib/my_app/administration/resources/administrator.ex -> `MyApp.Administration.Administrator`
- lib/my_app/administration/api.ex -> `MyApp.Administration.Api`
- lib/my_app/accounts/resources/user.ex -> `MyApp.Accounts.User` (not included in the example)
- lib/my_app/accounts/api.ex -> `MyApp.Accounts.Api` (not included in the example)

```elixir
# lib/my_app/administration/api.ex

defmodule MyApp.Administration.Api do
  use Ash.Api

  resources do
    resource MyApp.Administration.Administrator
  end
end

# in lib/my_app/administration/resources/administrator.ex

defmodule MyApp.Administration.Administrator do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Delegate

  delegate do
    # This resource will be backed by calls to the other API and resource
    to {MyApp.Accounts.Api, MyApp.Accounts.User}
    # When querying this resource, we include a filter by default on all calls
    # This lets us say `MyApp.Administration.Api.read(MyApp.Administration.Administrator)` to easily get
    # all adminstrators
    base_filter [admin: true]
  end

  # Define attributes/relationships as usual, using a subset (or all of) the delegated resource's attributes
  ...
end
```

Now we can add other resources and actions to our administration API, and they can use this more specific/appropriate variation of a user.

More will be coming on the topic of Domain Driven Design with Ash. Many features that will power it have yet to be written.
