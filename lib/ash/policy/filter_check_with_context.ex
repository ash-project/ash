defmodule Ash.Policy.FilterCheckWithContext do
  @moduledoc """
  A type of check that is represented by a filter statement, and has access to the context.
  """

  @type options :: Keyword.t()
  @type context :: %{
          required(:action) => Ash.Resource.Actions.action(),
          required(:resource) => Ash.Resource.t(),
          required(:domain) => Ash.Domain.t(),
          optional(:query) => Ash.Query.t(),
          optional(:changeset) => Ash.Changeset.t(),
          optional(any) => any
        }

  @callback filter(actor :: term, context(), options()) :: Keyword.t() | Ash.Expr.t()
  @callback reject(actor :: term, context(), options()) :: Keyword.t() | Ash.Expr.t()
  @optional_callbacks [reject: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Policy.FilterCheckWithContext
      @behaviour Ash.Policy.Check

      require Ash.Query

      def type, do: :filter

      def requires_original_data?(_, _), do: false

      def strict_check_context(opts) do
        []
      end

      def strict_check(nil, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)

        if Ash.Filter.template_references_actor?(filter(nil, authorizer, opts)) do
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

        actor
        |> filter(authorizer, opts)
        |> Ash.Filter.build_filter_from_template(actor, Ash.Policy.FilterCheck.args(authorizer))
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

      defp try_eval(expression, %{resource: resource, query: %Ash.Query{} = query}) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               aggregates: query.aggregates,
               calculations: query.calculations,
               public?: false
             }) do
          {:ok, hydrated} ->
            Ash.Expr.eval_hydrated(hydrated, resource: resource, unknown_on_unknown_refs?: true)

          {:error, error} ->
            {:error, error}
        end
      end

      defp try_eval(expression, %{
             resource: resource,
             changeset: %Ash.Changeset{action_type: :create} = changeset
           }) do
        case Ash.Filter.hydrate_refs(expression, %{
               resource: resource,
               unknown_on_unknown_refs?: true,
               aggregates: %{},
               calculations: %{},
               public?: false
             }) do
          {:ok, hydrated} ->
            Ash.Expr.eval_hydrated(hydrated, resource: resource, unknown_on_unknown_refs?: true)

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
            data =
              if changeset.context[:private][:pre_flight_authorization?] do
                data
              else
                nil
              end

            Ash.Expr.eval_hydrated(hydrated,
              record: data,
              resource: resource,
              unknown_on_unknown_refs?: true
            )

          {:error, error} ->
            {:error, error}
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
            Ash.Expr.eval_hydrated(hydrated, resource: resource, unknown_on_unknown_refs?: true)

          {:error, error} ->
            {:error, error}
        end
      end

      defp no_related_references?(expression) do
        expression
        |> Ash.Filter.list_refs()
        |> Enum.any?(&(&1.relationship_path != []))
      end

      def auto_filter(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)

        Ash.Filter.build_filter_from_template(
          filter(actor, authorizer, opts),
          actor,
          Ash.Policy.FilterCheck.args(authorizer)
        )
      end

      def auto_filter_not(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)

        Ash.Filter.build_filter_from_template(
          reject(actor, authorizer, opts),
          actor,
          Ash.Policy.FilterCheck.args(authorizer)
        )
      end

      def reject(actor, authorizer, opts) do
        opts = Keyword.put_new(opts, :resource, authorizer.resource)
        [not: filter(actor, authorizer, opts)]
      end

      def check(actor, data, authorizer, opts) do
        pkey = Ash.Resource.Info.primary_key(authorizer.resource)

        filter =
          case data do
            [record] -> Map.take(record, pkey)
            records -> [or: Enum.map(data, &Map.take(&1, pkey))]
          end

        authorizer.resource
        |> authorizer.domain.query()
        |> Ash.Query.filter(^filter)
        |> Ash.Query.filter(^auto_filter(actor, authorizer, opts))
        |> authorizer.domain.read()
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

      defoverridable reject: 3, requires_original_data?: 2
    end
  end

  def is_filter_check?(module) do
    :erlang.function_exported(module, :filter, 1)
  end
end
