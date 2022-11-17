defmodule Ash.Policy.FilterCheck do
  @moduledoc """
  A type of check that is represented by a filter statement

  That filter statement can be templated, currently only supporting `{:_actor, field}`
  which will replace that portion of the filter with the appropriate field value from the actor and
  `{:_actor, :_primary_key}` which will replace the value with a keyword list of the primary key
  fields of an actor to their values, like `[id: 1]`. If the actor is not present `{:_actor, field}`
  becomes `nil`, and `{:_actor, :_primary_key}` becomes `false`.

  You can customize what the "negative" filter looks like by defining `c:reject/1`. This is important for
  filters over related data. For example, given an `owner` relationship and a data layer like `ash_postgres`
  where `column != NULL` does *not* evaluate to true (see postgres docs on NULL for more):

      # The opposite of
      `owner.id == 1`
      # in most cases is not
      `not(owner.id == 1)`
      # because in postgres that would be `NOT (owner.id = NULL)` in cases where there was no owner
      # A better opposite would be
      `owner.id != 1 or is_nil(owner.id)`
      # alternatively
      `not(owner.id == 1) or is_nil(owner.id)`

  By being able to customize the `reject` filter, you can use related filters in your policies. Without it,
  they will likely have undesired effects.
  """
  @type options :: Keyword.t()
  @callback filter(options()) :: Keyword.t() | Ash.Expr.t()
  @callback reject(options()) :: Keyword.t() | Ash.Expr.t()
  @optional_callbacks [filter: 1, reject: 1]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Policy.FilterCheck
      @behaviour Ash.Policy.Check

      require Ash.Query

      def type, do: :filter

      def describe(opts) do
        inspect(filter(opts))
      end

      def strict_check_context(opts) do
        []
      end

      def strict_check(nil, authorizer, opts) do
        if Ash.Filter.template_references_actor?(opts[:filter]) do
          {:ok, false}
        else
          try_strict_check(nil, authorizer, opts)
        end
      end

      def strict_check(actor, authorizer, opts) do
        try_strict_check(actor, authorizer, opts)
      end

      defp try_strict_check(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)

        opts
        |> filter()
        |> Ash.Filter.build_filter_from_template(actor)
        |> try_eval(authorizer)
        |> case do
          {:ok, false} ->
            {:ok, false}

          {:ok, nil} ->
            {:ok, false}

          {:ok, _} ->
            {:ok, true}

          _ ->
            {:ok, :unknown}
        end
      end

      defp try_eval(expression, %{query: %Ash.Query{} = query}) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: query.resource,
               aggregates: query.aggregates,
               calculations: query.calculations,
               public?: false
             }) do
          {:ok, hydrated} ->
            Ash.Filter.Runtime.do_match(nil, hydrated)

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end

      defp try_eval(expression, %{
             resource: resource,
             changeset: %Ash.Changeset{action_type: :create} = changeset
           }) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, hydrated} ->
            if changeset.context[:private][:pre_flight_authorization?] do
              with {:no_related_refs, true} <-
                     {:no_related_refs, no_related_references?(expression)},
                   {:ok, fake_result} <- Ash.Changeset.apply_attributes(changeset, force?: true) do
                Ash.Filter.Runtime.do_match(fake_result, hydrated)
              else
                {:no_related_refs, false} ->
                  :unknown

                {:error, error} ->
                  {:halt, {:error, error}}
              end
            else
              Ash.Filter.Runtime.do_match(nil, hydrated)
            end

          {:error, error} ->
            {:error, error}
        end
      end

      defp try_eval(expression, %{
             resource: resource,
             changeset: %Ash.Changeset{data: data} = changeset
           }) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, hydrated} ->
            # We don't want to authorize on stale data in real life
            # but when using utilities to check if something *will* be authorized
            # that is our intent
            if changeset.context[:private][:pre_flight_authorization?] do
              Ash.Filter.Runtime.do_match(data, hydrated)
            else
              Ash.Filter.Runtime.do_match(nil, hydrated)
            end

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end

      defp try_eval(expression, %{resource: resource}) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, hydrated} ->
            Ash.Filter.Runtime.do_match(nil, hydrated)

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end

      defp no_related_references?(expression) do
        expression
        |> Ash.Filter.list_refs()
        |> Enum.any?(&(&1.relationship_path != []))
      end

      def auto_filter(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)
        Ash.Filter.build_filter_from_template(filter(opts), actor)
      end

      def auto_filter_not(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)
        Ash.Filter.build_filter_from_template(reject(opts), actor)
      end

      def reject(opts) do
        [not: filter(opts)]
      end

      def check(actor, data, authorizer, opts) do
        pkey = Ash.Resource.Info.primary_key(authorizer.resource)

        filter =
          case data do
            [record] -> Map.take(record, pkey)
            records -> [or: Enum.map(data, &Map.take(&1, pkey))]
          end

        authorizer.resource
        |> authorizer.api.query()
        |> Ash.Query.filter(^filter)
        |> Ash.Query.filter(^auto_filter(authorizer.actor, authorizer, opts))
        |> authorizer.api.read()
        |> case do
          {:ok, authorized_data} ->
            authorized_pkeys = Enum.map(authorized_data, &Map.take(&1, pkey))

            Enum.filter(data, fn record ->
              Map.take(record, pkey) in authorized_pkeys
            end)

          {:error, error} ->
            {:error, error}
        end
      end

      defoverridable reject: 1, describe: 1
    end
  end

  def is_filter_check?(module) do
    :erlang.function_exported(module, :filter, 1)
  end
end
