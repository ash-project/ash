defmodule Ash.DataLayer do
  @callback filter(Ash.query(), field :: atom, value :: term, resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback limit(Ash.query(), limit :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback offset(Ash.query(), offset :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback resource_to_query(Ash.resource()) :: Ash.query()
  @callback relationship_query(Ash.record() | list(Ash.record()), Ash.relationship()) ::
              Ash.query()
  @callback can_query_async?(Ash.resource()) :: boolean
  @callback run_query(Ash.query(), Ash.resource()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}

  # @callback create(
  #             Ash.resource(),
  #             Ash.action(),
  #             Ash.attributes(),
  #             Ash.relationships(),
  #             Ash.params()
  #           ) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}

  # @callback update(
  #             Ash.record(),
  #             Ash.action(),
  #             Ash.attributes(),
  #             Ash.relationships(),
  #             Ash.params()
  #           ) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}

  # @callback delete(Ash.record(), Ash.action(), Ash.params()) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}

  # @callback append_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}

  # @callback delete_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}

  # @callback replace_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #             {:ok, Ash.record()} | {:error, Ash.error()}
end
