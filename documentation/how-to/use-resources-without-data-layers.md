# Use Resources without Data Layers

If you don't explicitly set a data layer, then a resource will use `Ash.DataLayer.Simple`. In this way, technically, a resource _always_ has a data layer. `Ash.DataLayer.Simple` is a data layer that does no persistence, but instead validates and returns structs for mutating actions, and must be manually provided with data to use.

## Example

```elixir
defmodule MyApp.Person do
  use Ash.Resource
  # notice no data layer is configured

  attributes do
    uuid_primary_key :id
    attribute :name, :string, allow_nil?: false, public?: true
    attribute :points, :integer, allow_nil?: false, public?: true
  end

  actions do
    read :read do
      prepare MyApp.FetchPeople
    end

    create :create do
      accept [:some]
    end
  end
end

defmodule MyApp.FetchPeople do
  use Ash.Resource.Preparation

  @fake_people [
    %MyApp.Person{
      id: Ash.UUID.generate(),
      name: "Joe Armstrong",
      points: 10000
    },
    %MyApp.Person{
      id: Ash.UUID.generate(),
      name: "José Valim",
      points: 10000
    }
  ]

  def prepare(query, _, _) do
    Ash.Query.before_action(query, fn query ->
      case fetch_data(query) do
        {:ok, data} ->
          Ash.DataLayer.Simple.set_data(query, data)
        {:error, error} ->
          Ash.Query.add_error(query, SomeBuiltinOrCustomAshError.exception(...))
      end
    end)
  end

  defp fetch_data(_query) do
    # you could fetch them from an external API here, but for this example
    # we will just return some static data.
    # Be sure to return instances of the resource!

    {:ok, @fake_people}
  end
end
```

## Usage

They are used in exactly the same way as regular resources

## Create/Update/Destroy

In the example below, we create one, although it is not persisted anywhere and will not be returned when reading. However, you could do custom persistence. If, for example, we were reading from an external API, you might post to an API in an after_action hook on the create.

```elixir
# You can construct changeset over them
changeset =
Ash.Changeset.for_create(MyApp.Person, :create, %{name: "Dave Thomas", points: 10000})

# This will return the structs by default
# Although you are free to do custom persistence in your resource changes
Ash.create!(changeset)
# %MyApp.FetchComplexResource{...}
```

## Reads

When reading, you can use the resource as you would any other resource.

```elixir
Resource
|> Ash.Query.filter(contains(name, "José"))
|> Ash.read!()
#=> [%MyApp.Person{name: "José Valim", points: 10000}]
```

Notice how consumers of your resource (generally) don't need to care if the data is coming from a database, an external API, or static data somewhere. They get the same API, and you can do things like pagination, sorting, etc. in the same way. You can even add the `AshGraphql` or `AshJsonApi` extension to expose an external API in your _own_ API!
