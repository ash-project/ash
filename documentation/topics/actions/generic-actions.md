# Generic Actions

Generic actions are so named because there are no special rules about how they work. A generic action takes arguments and returns a value. The struct used for building input for a generic action is `Ash.ActionInput`.

```elixir
action :say_hello, :string do
  argument :name, :string, allow_nil?: false

  run fn input, _ ->
    {:ok, "Hello: #{input.arguments.name}"}
  end
end
```

A generic action declares its arguments, return type, and implementation, as illustrated above.

> ### No return? No problem! {: .tip}
>
> Generic actions can omit a return type, in which case running them returns `:ok` if successful.
>
> ```elixir
> action :schedule_job do
>   argument :job_name, :string, allow_nil?: false
>   run fn input ->
>     # Schedule the job
>     :ok
>   end
> end
> ```

## Why use generic actions?

The example above could be written as a normal function in elixir, i.e

```elixir
def say_hello(name), do: "Hello: #{name}"
```

The benefit of using generic actions instead of defining normal functions:

- They can be used with api extensions like `ash_json_api` and `ash_graphql`
- Their inputs are type checked and casted
- They support Ash authorization patterns (i.e policies)
- They can be included in the code interface of a resource
- They can be made transactional with a single option (`transaction? true`)

If you don't need any of the above, then there is no problem with writing regular Elixir functions!

## Return types and constraints

Generic actions do not cast their return types. It is expected that the action return a valid value for the type that they declare. However, declaring additional constraints can inform API usage, and make the action more clear. For example:

```elixir
action :priority, :integer do
  constraints [min: 1, max: 3]
  argument :status, :atom, constraints: [one_of: [:high, :medium, :low]]

  run fn input, _ ->
    case input.arguments.status do
      :high -> {:ok, 3}
      :medium -> {:ok, 2}
      :low -> {:ok, 1}
    end
  end
end
```

> #### Returning resource instances {: .info}
>
> It sometimes happens that you want to make a generic action which returns an
> instance or instances of the resource. It's natural to assume that you can
> set your action's return type to the name of your resource. This won't work
> as resources do not define a type, unless they are embedded. In embedded resources, this won't work because the module is still being compiled, so referencing yourself as a type causes a compile error. Instead, use the `:struct` type and the `instance_of` constraint, like so:
> ```elixir
> action :get, :struct do
>   constraints instance_of: __MODULE__
>
>   run # ...
> end
> ```
