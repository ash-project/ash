# Defining Manual Relationships

Manual relationships allow for expressing complex/non-typical relationships between resources in a standard way.
Individual data layers may interact with manual relationships in their own way, so see their corresponding guides.

By default, the only thing manual relationships support is being loaded.

## Example

```elixir
# in the resource

relationships do
  has_many :tickets_above_threshold, Helpdesk.Support.Ticket do
    manual Helpdesk.Support.Ticket.Relationships.TicketsAboveThreshold
  end
end

# implementation
defmodule Helpdesk.Support.Ticket.Relationships.TicketsAboveThreshold do
  use Ash.Resource.ManualRelationship
  require Ash.Query

  def load(records, _opts, %{query: query, actor: actor, authorize?: authorize?}) do
    # Use existing records to limit resultds
    rep_ids = Enum.map(records, & &1.id)
     # Using Ash to get the destination records is ideal, so you can authorize access like normal
     # but if you need to use a raw ecto query here, you can. As long as you return the right structure.

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