# Managing Relationships

In Ash, managing related data is done via `Ash.Changeset.manage_relationship/4`. There are various ways to leverage the functionality expressed there. If you are working with changesets directly, you can call that function. However, if you want that logic to be portable (e.g available in `ash_graphql` mutations and `ash_json_api` actions), then you want to use the following `argument` + `change` pattern:

```elixir
actions do
  update :update do
    argument :add_comment, :map do
      allow_nil? false
    end

    argument :tags, {:array, :uuid} do
      allow_nil? false
    end

    # First argument is the name of the action argument to use
    # Second argument is the relationship to be managed
    # Third argument is options. For more, see `Ash.Changeset.manage_relationship/4`. This accepts the same options.
    change manage_relationship(:add_comment, :comments, type: :create)

    # Second argument can be omitted, as the argument name is the same as the relationship
    change manage_relationship(:tags, type: :append_and_remove)
  end
end
```

With this, those arguments can be used in action input:

```elixir
post
|> Ash.Changeset.for_update(:update, tags: [tag1.id, tag2.id], add_comment: %{text: "comment text"})
|> MyDomain.update!()
```

## Argument Types

Notice how we provided a map as input to `add_comment`, and a list of UUIDs as an input to `manage_relationship`. When providing maps or lists of maps, you are generally just providing input that will eventually be passed into actions on the destination resource. However, you can also provide individual values or lists of values. By default, we assume that value maps to the primary key of the destination resource, but you can use the `value_is_key` option to modify that behavior. For example, if you wanted adding a comment to take a list of strings, you could say:

```elixir
argument :add_comment, :string 

...
change manage_relationship(:add_comment, :comments, type: :create, value_is_key: :text)
```

And then you could use it like so:

```elixir
post
|> Ash.Changeset.for_update(:update, tags: [tag1.id, tag2.id], add_comment: "comment text")
|> MyDomain.update!()
```

## Derived behavior

Determining what will happen when managing related data can be complicated, as the nature of the problem itself is quite complicated. In some simple cases, like `type: :create`, there may be only one action that will be called. But in order to support all of the various ways that related resources may need to be managed, Ash provides a very rich set of options to determine what happens with the provided input. Tools like `AshPhoenix.Form` can look at your arguments that have a corresponding `manage_relationship` change, and derive the structure of those nested forms. Tools like `AshGraphql` can derive complex input objects to allow manipulating those relationships over a graphql Api. This all works because the options are, ultimately, quite explicit. It can be determined exactly what actions might be called, and therefore what input could be needed.