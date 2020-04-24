defmodule Ash.Authorization.Check.RelationshipSet do
  use Ash.Authorization.Check, action_types: [:create, :update, :read, :delete]

  @impl true
  def describe(opts) do
    "#{opts[:relationship_name]} is already set"
  end

  @impl true
  # TODO: Add a filter for "has_something_related", and then check for that here for read actions
  # TODO: Make this support a nested relationship path?                                         
  def strict_check(_user, _request, _options) do
    {:ok, :unknown}
  end

  @impl true
  def prepare(opts) do
    [side_load: opts[:relationship_name]]
  end

  @impl true
  def check(_user, records, _state, opts) do
    Enum.reject(records, fn record ->
      Map.get(record, opts[:relationship_name]) in [nil, []]
    end)
  end
end
