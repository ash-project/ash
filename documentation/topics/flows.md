# Flows

A flow is a static definition of a set of steps to be run.

Flows are backed by `executors`, which determine how the workflow steps are performed.
The executor can be overriden on invocation, but not all executors will be capable of running all flows.
As of this writing, the default executor is the only one. It runs all steps in parallel unless values must be provided from one step to another, or in steps that are enclosed by a transaction.

Ash.Flow is still in its early days, so expect many features, step types, and executors to come in the future.

All explanations here pertain to the builtin executor, so be sure to read the documentation of any other executor you may use.

Flows are comprised of steps, which each have an `input` and an `result`. By default, each step is executed concurrently (or at least *may* be executed concurrently). When the result of one step is used in another, that will cause them to run in sequence. In the following flow, for example, the `:create_user` and `:create_blank_project` steps would happen concurrently, but both would wait on the `:create_org` step.


```elixir
flow do
  # Flow arguments allow you to parameterize the flow
  argument :org_name, :string do
    allow_nil? false
  end

  argument :user_name, :string do
    allow_nil? false
  end

  # The flow returns the result of the `:create_user` step.
  returns :create_user
end

steps do
  # The step is called `:create_org`, and it creates an `Organization` using the `register_org` action.
  create :create_org, MyApp.Accounts.Organization, :register_org do
    # The input to the action refers to an argument of the flow
    input %{
      name: arg(:org_name)
    }
  end

  # The step is called :create_user, and it creates a `User` using the `:register_user` action.
  create :create_user, MyApp.Accounts.User, :register_user do
    input %{
      # The input refers to an argument of the flow
      name: arg(:user_name),
      # and to the result of another step
      org: result(:create_org)
    }
  end

  # The step is called :create_blank_project, and it creates a `Project` using the `:register_user` action.
  create :create_blank_project, MyApp.Accounts.Project, :create_example do
    input %{
      # The input refers to the result of another step
      org: result(:create_org)
    }
  end
end
```

## Return Values

`returns` determines what the flow returns, and may be one of three things:

- `:step_name` - will return the result of the configured step
- `%{step_name: :key}` will return a map of each key to the provided step name, i.e `%{key: <step_name_result>}`
- `[:step_name]` - which is equivalent to `%{step_name: :step_name}`

## Errors

Currently, any error anywhere in the flow will simply fail the flow and will return an error. Over time, error handling behavior will be added, as well as the ability to customize how transactions are rolled back, and to handle errors in a custom way.

## Custom steps

Custom steps allow you to implement any custom logic that you need. There aren't really any restrictions on what you do in a custom step, but there is one main consideration if you want your custom step to play nicely with transactions:

Generally speaking you should set the `touches_resources` if you set `async?` to true.
This ensures that the custom step will be run synchronously if any of those resource's data
layers is in a corresponding transaction. You don't necessarily need to set *all* of the
resources that will be touched. For example, all AshPostgres resources that share the same
repo share the same transaction state.