# Timeouts

Timeouts in Ash work a bit differently than other tools. The following considerations must be taken into account:

1. If you run a resource action in a transaction, then the timeout applies to the entire transaction.
2. If the resource action you are running, and any of its `touches_resources` is *already in a transaction* then the timeout is ignored, as the outer transaction is handling the timeout.
3. If the resource is not in a transaction, then the timeout is applied to each query that is run. Ash may run many queries to fulfill the action, so keep in mind that the request can take *considerably longer* than the specified timeout. No single query will be allowed to take longer than that timeout, however. The practical implications here is that `create/update/destroy` actions with a timeout will honor that timeout because they default to `transaction?: true`. If you wish to specify a holistic timeout for read actions, then you must specify `transaction?: true, timeout: timeout`.
4. If the data layer of the resource does not support timeouts, then timeouts are **ignored** and an error is returned if one is specified.
5. As of the writing of this guide, none of the API extensions support specifying a timeout, but the general idea is that if you need "holistic" timeouts for an action or a request, you have two options: if you are calling your resources with hand written code, run the code in a `Task` and await it with a timeout. If you are using an API extension, add the timeouts to the configuration of those extensions, as they can easily be configured to run requests in a `Task` with a timeout in the same way.

## Ways to Specify Timeouts

You have a few options.

You can specify a timeout when you call an action. This takes the highest precedence.

```elixir
MyApi.read!(query, timeout: :timer.seconds(30))
```

You can specify one using `Ash.Changeset.timeout/2` or `Ash.Query.timeout/2`. This can be useful if you want to conditionally set a timeout based on the details of the request. For example, you might:

```elixir
# in your resource
defmodule MyApp.SetReportTimeout do
  use Ash.Resource.Preparation

  def prepare(query, _, _) do
    if Ash.Query.get_argument(query, :full_report) do
      Ash.Query.timeout(query, :timer.minutes(3))
    else
      Ash.Query.timeout(query, :timer.minutes(1))
    end
  end
end

actions do
  read :report_items do
    argument :full_report, :boolean, default: false

    prepare MyApp.SetReportTimeout
  end
end
```

And you can specify a default timeout on the Api module that you call your resources with. Overriding an api with a default timeout requires providing a timeout of `:infinity` in one of the other methods.

```elixir
execution do
  timeout :timer.seconds(30) # this is the default
end
```