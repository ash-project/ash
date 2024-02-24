# Timeouts

Timeouts in Ash work a bit differently than other tools. The following considerations must be taken into account:

1. If you run a resource action in a transaction, then the timeout applies to the entire transaction.
2. If the resource action you are running, and any of its `touches_resources` is *already in a transaction* then the timeout is ignored, as the outer transaction is handling the timeout.
3. If the resource is not in a transaction, and supports async execution (ash_postgres does), then everything is run in a task and awaited with the provided timeout.
4. If the data layer of the resource does not support timeouts, or async execution then timeouts are **ignored**.
5. As of the writing of this guide, none of the web API extensions support specifying a timeout. If/when they do, they will run the action they are meant to run in a `Task`.

## Ways to Specify Timeouts

You have a few options.

You can specify a timeout when you call an action. This takes the highest precedence.

```elixir
Ash.read!(query, timeout: :timer.seconds(30))
```

You can specify one using `Ash.Changeset.timeout/2` or `Ash.Query.timeout/2`. This can be useful if you want to conditionally set a timeout based on the details of the request. For example, you might do something like this:

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

And you can specify a default timeout on the domain module that you call your resources with. Overriding a domain with a default timeout requires providing a timeout of `:infinity` in one of the other methods.

```elixir
execution do
  timeout :timer.seconds(30) # this is the default
end
```

Keep in mind, you can't specify timeouts in a before_action or after_action hook, because at that point you are already "within" the code that should have a timeout applied.