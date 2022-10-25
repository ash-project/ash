# PubSub

Ash includes a builtin notifier to help you publish events over any kind of pub-sub pattern. This is plug and play with `Phoenix.PubSub`, but could be used with any pubsub pattern. 

You configure a module that defines a `broadcast/3` function, and then add some "publications" which configure under what conditions an event should be sent and what the topic should be.

## Topic Templates

Often you want to include some piece of data in the thing being changed, like the `:id` attribute. This is done by providing a list as the topic, and using atoms which will be replaced by their corresponding values. They will ultimately be joined with `:`.

For example:

```elixir
prefix "user"

publish :create, ["created", :user_id]
```

This might publish a message to "user:created:1" for example.

For updates, if the field in the template is being changed, a message is sent
to *both* values. So if you change `user 1` to `user 2`, the same message would
be published to `user:updated:1` and `user:updated:2`. If there are multiple
attributes in the template, and they are all being changed, a message is sent for
every combination of substitutions.

## Template parts

Templates may contain lists, in which case all combinations of values in the list will be used. Add
`nil` to the list if you want to produce a pattern where that entry is omitted.

The atom `:_tenant` may be used. If the changeset has a tenant set on it, that
value will be used, otherwise that combination of values is ignored.

The atom `:_pkey` may be used. It will be a stringified, concatenation of the primary key fields,
or just the primary key if there is only one primary key field.

The atom `nil` may be used. It only makes sense to use it in the context of a list of alternatives,
and adds a pattern where that part is skipped.

```elixir
publish :updated, [[:team_id, :_tenant], "updated", [:id, nil]]
```

Would produce the following messages, given a `team_id` of 1, a `tenant` of `org_1`, and an `id` of `50`:

```elixir
"1:updated:50"
"1:updated"
"org_1:updated:50"
"org_1:updated"
```

## Usage with Phoenix

Phoenix expects a specific shape of data to be broadcasted, and since it is so often used with Ash, instead of making you define your own notifier that creates the `%Phoenix.Socket.Broadcast` struct and publishes it, Ash has an option to do that automatically, via

```elixir
broadcast_type: :phoenix_broadcast
```