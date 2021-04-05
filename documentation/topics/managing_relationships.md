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

    # Second argument can be ommitted, as the argument name is the same as the relationship
    change manage_relationship(:tags, type: :replace)
  end
end
```

With this, those arguments can be used simply in action input: 

```elixir
post
|> Ash.Changeset.for_update(:update, tags: [tag1_uuid, tag2_uuid], add_comment: %{text: "comment text"})
|> MyApi.update!()
```

It gets even simpler if you are using the `code_interface`, for example:

```elixir
# With this in your resource
code_interface do
  define :update_post, action: :update
end

# You can use it like so:

MyApi.update_post!(%{tags: [tag1_uuid, tag2_uuid], add_comment: %{text: "comment text"}})
```

## Argument Types

Notice how we provided a map as input to `add_comment`. The only types supported by `manage_relationship` are values that map to the primary key of the resource, which is why `tags` allowed the list of `:uuid`s. However, `%{text: "comment text"}` must be a map,
as it will eventually be passed to a create action on the `Comment` resource. The ergonomics of this are still being worked out, but there are ways to make it such that your action accepts input like `add_comment: "comment text"`. For now, the only way to do it would be by adding a private argument to hold the proper input for `add_comment`, and a change to set that argument, based on the provided value. For example:

```elixir
defmodule MyApp.Post.Changes.SetAddCommentArgument do
  use Ash.Resource.Change


  def change(changeset, _, _) do
    case Ash.Changeset.fetch_argument(changeset, :add_comment) do
      {:ok, comment_text} -> Ash.Changeset.set_argument(changeset, :private_add_comment, %{text: comment_text})
      :error -> changeset
    end
  end
end

actions do
  update :update do
    argument :add_comment, :string do
      allow_nil? false
    end

    argument :private_add_comment, :map do
      # Extensions know not to expose private arguments
      private? true
    end

    change MyApp.Post.Changes.SetAddCommentArgument

    change manage_relationship(:private_add_comment, :comments, type: :create)
  end
end
```

## Graphql Input Types

In `ash_graphql`, a type of `:map` simply translates to `:json`. Right now, there is nothing that can automatically generate the requisite input object for a given argument that eventually gets passed to `manage_relationship/3`. So if you want typed input objects to use with those arguments, you will need to use a custom map type implementation, and have it refer to a custom `absinthe` type. Thankfully, `absinthe` makes it very easy to define new input_object types. For example:

```elixir
defmodule MyApp.Types.CreateCommentInput do
  use Ash.Type

  def graphql_input_type, do: :create_comment_input

  defdelegate storage_type, to: Ash.Type.Map
  defdelegate cast_input(value, constraints), to: Ash.Type.Map
  defdelegate cast_stored(value, constraints), to: Ash.Type.Map
  defdelegate dump_to_native(value, constraints), to: Ash.Type.Map
end
```

Given that type definition, you could then add the following to your absinthe schema:

```elixir
input_object :create_comment_input do
   field :text, :string
end
```

We're open to suggestions on making this process more ergonomic in general.