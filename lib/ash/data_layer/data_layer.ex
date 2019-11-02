defmodule Ash.DataLayer do
  @callback relationship_query(Ash.record(), Ash.relationship()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback filter(Ash.query(), field :: atom, value :: term, central_resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}

  @callback limit(Ash.query(), limit :: non_neg_integer(), central_resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback offset(Ash.query(), offset :: non_neg_integer(), central_resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback resource_to_query(Ash.resource()) :: {:ok, Ash.query()} | {:error, Ash.error()}
  @callback get_one(Ash.query(), central_resource :: Ash.resource()) ::
              {:ok, nil | Ash.record()} | {:error, Ash.error()}
  @callback get_many(Ash.query(), central_resource :: Ash.resource()) ::
              {:ok, [Ash.record()]} | {:error, Ash.error()}
  @callback side_load([Ash.record()], Ash.side_load_keyword(), Ash.resource()) ::
              {:ok, [Ash.resource()]} | {:error, Ash.error()}
  @callback create(
              Ash.resource(),
              Ash.action(),
              Ash.attributes(),
              Ash.relationships(),
              Ash.params()
            ) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @callback update(
              Ash.record(),
              Ash.action(),
              Ash.attributes(),
              Ash.relationships(),
              Ash.params()
            ) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @callback delete(Ash.record(), Ash.action(), Ash.params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @callback append_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @callback delete_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @callback replace_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}
end
