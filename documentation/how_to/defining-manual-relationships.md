# Defining Manual Relationships

Manual relationships allow you to express complex or non-typical relationships between resources in a standard way.
Individual data layers may interact with manual relationships in their own way, so see their corresponding guides.

By default, the only thing manual relationships support is being loaded.

## Example

In our Helpdesk example, we'd like to have a way to find tickets

In the `Rep?` resource, define a `has_many` relationship as `manual` and point to the module where
it will be implemented.

```elixir
relationships do
  has_many :tickets_above_threshold, Helpdesk.Support.Ticket do
    manual Helpdesk.Support.Ticket.Relationships.TicketsAboveThreshold
  end
end
```

Using Ash to get the destination records is ideal, so you can authorize access like normal
but if you need to use a raw ecto query here, you can. As long as you return the right structure.

The `TicketsAboveThreshold` module is implemented as follows.

```elixir
defmodule Helpdesk.Support.Ticket.Relationships.TicketsAboveThreshold do
  use Ash.Resource.ManualRelationship
  require Ash.Query

  def load(records, _opts, %{query: query, actor: actor, authorize?: authorize?}) do
    # Use existing records to limit resultds
    rep_ids = Enum.map(records, & &1.id)

    {:ok,
     query
     |> Ash.Query.filter(representative_id in ^rep_ids)
     |> Ash.Query.filter(priority > representative.priority_threshold)
     |> Helpdesk.Support.read!(actor: actor, authorize?: authorize?)
     # Return the items grouped by the primary key of the source, i.e representative.id => [...tickets above threshold]
     |> Enum.group_by(& &1.representative_id)}
  end
end
```

## Using the Query

Since you likely want to support things like filtering your relationship when being loaded, you will want to make sure that you use the query being provided. However, depending on how you're loading the relationship, you may need to do things like fetch extra records. To do this, you might do things like

```elixir
def load(records, _opts, %{query: query, ..}) do
  # unset some fields
  fetch_query = Ash.Query.unset(query, [:limit, :offset])

  # or, to be more safe/explicit, you might make a new query, explicitly setting only a few fields
  fetch_query = query.resource |> Ash.Query.filter(^query.filter) |> Ash.Query.sort(query.sort)

  ...
end
```
