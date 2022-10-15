# Use Without Data Layers

If a resource is configured without a data layer, then it will always be working off of a temporary data set that lives only for the life of that query. This can be a powerful way to model input validations and/or custom/complex reads. Technically, resources without a data layer use `Ash.DataLayer.Simple`, which does no persistence, and expects to find any data it should use for read actions in a context on the query

## Example

```elixir
defmodule MyApp.MyComplexResource do
  use Ash.Resource
  # notice no data layer is configured
  
  attributes do
    #A primary key is always necessary on a resource, but this will generate one for you automatically
    uuid_primary_key :id
    attribute :some_complex_derived_number, :integer
  end

  actions do
    read :read do
      prepare MyApp.FetchComplexResources
    end

    create :validate_input do
       ...
       # will validate required inputs, and you can add 
       # validations like you would for any normal resource
    end
  end
end

defmodule MyApp.FetchComplexResources do
  use Ash.Resource.Preparation
 
  def prepare(query, _, _) do
    case fetch_data(query) do
      {:ok, data} ->
        Ash.DataLayer.Simple.set_data(query, data)
      {:error, error} ->
        Ash.Query.add_error(query, SomeBuiltinOrCustomAshError.exception(...))
    end
  end
end
```

## Usage

They are used in exactly the same way as regular resources

```elixir
# You can construct changeset over them
changeset =
Ash.Changeset.for_create(MyApp.FetchComplexResource, :validate_input, %{})

# This will return the structs by default
# Although you are free to do custom persistence in your resource changes
MyApp.MyApi.create!(changeset)
# %MyApp.FetchComplexResource{...}
```
