

    To include attribute values of the resource in the message, pass a list
    of strings and attribute names. They will ultimately be joined with `:`.
    For example:

    ```elixir
    prefix "user"

    publish :create, ["created", :user_id]
    ```

    This might publish a message to \"user:created:1\"" for example.

    For updates, if the field in the template is being changed, a message is sent
    to *both* values. So if you change `user 1` to `user 2`, the same message would
    be published to `user:updated:1` and `user:updated:2`. If there are multiple
    attributes in the template, and they are all being changed, a message is sent for
    every combination of substitutions.

    ## Template parts

    Templates may contain lists, in which case all combinations of values in the list will be used. Add
    `nil` to the list if you want to produce a pattern where that entry is ommitted.

    The atom `:_tenant` may be used. If the changeset has a tenant set on it, that
    value will be used, otherwise that combination of values is ignored.

    The atom `:_pkey` may be used. It will be a stringified, concatenation of the primary key fields,
    or just the primary key if there is only one primary key field.

    The atom `:_skip` may be used. It only makes sense to use it in the context of a list of alternatives,
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