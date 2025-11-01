<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Backwards Compatibility Config

All of these configurations are potentially breaking changes when applied
to your application. However, we *highly* encourage setting as many of
them as possible. In 4.0, some will be removed entirely, and any that remain
will have their defaults changed to the new value.

The ash installer automatically sets all of these.

## allow_forbidden_field_for_relationships_by_default?

```elixir
config :ash, allow_forbidden_field_for_relationships_by_default?: true
```

### Old Behavior

Loaded relationships that produced a `Forbidden` error would fail the entire
request. i.e in `Ash.load(post, [:comments, :author])`, if `author` returned
a `Forbidden` error, the entire request would fail with a forbidden error.

### New Behavior

Now the relationships that produced a forbidden error are instead populated
with `%Ash.ForbiddenField{}`.

## include_embedded_source_by_default?

```elixir
config :ash, include_embedded_source_by_default?: false
```

### Old Behavior

When working with embedded types, the `__source__` constraint is populated with
the original changeset. This can be very costly in terms of memory when working with
large sets of embedded resources.

### New Behavior

Now, the source is only included when you say `constraints: [include_source?: true]` on
the embedded resource's usage.

## show_keysets_for_all_actions?

```elixir
config :ash, show_keysets_for_all_actions?: false
```

### Old Behavior

For all actions, the records would be returned with `__metadata__.keyset` populated
with a keyset computed for the `sort` that was used to produce those records. This
is expensive as it requires loading all things that are used by the sort.

### New Behavior

Only when actually performing keyset pagination will the `__metadata__.keyset` be
computed.

## default_page_type

```elixir
config :ash, default_page_type: :keyset
```

### Old Behavior

When an action supports both `offset` and `keyset` pagination, and a page is requested
with only `limit` set (i.e., `page: [limit: 10]`), Ash defaulted to offset pagination
and returned an `%Ash.Page.Offset{}`.

### New Behavior

With the current default configuration, Ash will now return an `%Ash.Page.Keyset{}` when the pagination
type is ambiguous (only `limit` is provided).

For detailed pagination behavior documentation, see the [pagination guide](/documentation/topics/advanced/pagination.livemd#default-pagination-behavior-when-both-types-are-supported).

## policies.no_filter_static_forbidden_reads?

```elixir
config :ash, policies: [no_filter_static_forbidden_reads?: false]
```

### Old Behavior

On read action policies, we can often tell statically that they cannot pass, for example:

```elixir
policy action_type(:read) do
  authorize_if actor_attribute_equals(:active, true)
end
```

In these cases, you would get an `Ash.Error.Forbidden`, despite the fact that the
default `access_type` for a policy is `:filter`. If you instead had:

```elixir
policy action_type(:read) do
  authorize_if expr(private == false)
end
```

You would get a filter. This made it difficult to predict when you would get a forbidden
error and when the query results would  be filtered.


### New Behavior

Now, we always filter the query even if we know statically that the request would be
forbidden. For example the following policy:

```elixir
policy action_type(:read) do
  authorize_if actor_attribute_equals(:active, true)
end
```

would yield `filter: false`. This makes the behavior consistent and predictable.
You can always annotate that a given policy should result in a forbidden error
by setting `access_type :strict` in the policy.

## keep_read_action_loads_when_loading?

```elixir
config :ash, keep_read_action_loads_when_loading?: false
```

### Old Behavior

If you had an action with a preparation, or a global preparation that loaded data, i.e

```elixir
prepare build(load: :comments)
```

this wold be applied when using `Ash.load`, because we build a query for the primary
read action as a basis for loading data. This could be expensive because now you are always
loading `:comments` even if you only intended to load something else, and could also be
unpredictable because it could "overwrite" the already loaded `comments` on the data you
passed in.

### New Behavior

When using `Ash.load` *only* the explicitly provided load statement is applied.

## default_actions_require_atomic?

```elixir
config :ash, default_actions_require_atomic?: true
```

### Old Behavior

When building actions like so: `defaults [:read, create: :*, update: :*]` the default
action is generated with `require_atomic? false`. This could make it difficult to spot
actions that cannot safely be done asynchronously.

### New Behavior

The default generated actions are generated with `require_atomic? true`

## read_action_after_action_hooks_in_order?

```elixir
config :ash, read_action_after_action_hooks_in_order?: true
```

### Old Behavior

In 3.0, we modified hooks on changesets to always be added in order instead of in
reverse order. This was missed for `Ash.Query`. Meaning if you had something like this:

```elixir
read :read do
  prepare fn query, _ ->
    Ash.Query.after_action(query, fn query, results ->
      IO.puts("hook 1")
      {:ok, results}
    end)
  end

  prepare fn query, _ ->
    Ash.Query.after_action(query, fn query, results ->
      IO.puts("hook 2")
      {:ok, results}
    end)
  end
end
```

running that action would print `hook 2` before `hook 1`.

### New Behavior

Read action hooks are now run in the order they were added

## bulk_actions_default_to_errors?

```elixir
config :ash, bulk_actions_default_to_errors?: true
```

### Old Behavior

Bulk action options defaulted to `return_errors?: false`, and `stop_on_error?: false`,
which was often a footgun for users unfamiliar to bulk actions, wondering "why did I not
get an error even though nothing was created?"

### New Behavior

Now, `return_errors?` and `stop_on_error?` default to `true`


## match_v4_uuids?

```elixir
config :ash, Ash.Type.UUIDv7, match_v4_uuids?: true
```

### Old Behavior

Incorrectly allowed non UUIDv7's to be loaded.

### New Behavior

Configuring the variable allows UUIDv4's to be loaded.
