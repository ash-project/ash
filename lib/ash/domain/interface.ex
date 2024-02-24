defmodule Ash.Domain.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
      alias Ash.Domain

      @spec can?(
              query_or_changeset_or_action ::
                Ash.Query.t()
                | Ash.Changeset.t()
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()},
              actor :: term,
              opts :: Keyword.t()
            ) ::
              boolean | no_return
      def can?(query_or_changeset_or_action, actor, opts \\ []) do
        Domain.can?(__MODULE__, query_or_changeset_or_action, actor, opts)
      end

      @spec can(
              action_or_query_or_changeset ::
                Ash.Query.t()
                | Ash.Changeset.t()
                | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()},
              actor :: term,
              opts :: Keyword.t()
            ) ::
              {:ok, boolean | :maybe} | {:error, term}
      def can(action_or_query_or_changeset, actor, opts \\ []) do
        Domain.can(__MODULE__, action_or_query_or_changeset, actor, opts)
      end

      def run_action!(input, opts \\ []) do
        Domain.run_action!(__MODULE__, input, opts)
      end

      def run_action(input, opts \\ []) do
        Domain.run_action(__MODULE__, input, opts)
      end

      def calculate!(resource, calculation, opts \\ []) do
        case calculate(resource, calculation, opts) do
          {:ok, result} ->
            result

          {:error, error} ->
            raise error
        end
      end

      def calculate(resource, calculation, opts \\ []) do
        case Domain.calculate(resource, calculation, opts) do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def aggregate!(query, aggregate_or_aggregates, opts \\ []) do
        case aggregate(query, aggregate_or_aggregates, opts) do
          {:ok, result} ->
            result

          {:error, error} ->
            raise error
        end
      end

      def count!(query, opts \\ []) do
        query = Ash.Query.to_query(query)

        opts =
          if query.action do
            Keyword.put(opts, :read_action, query.action.name)
          else
            opts
          end

        {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

        case Domain.aggregate(__MODULE__, query, {:count, :count, aggregate_opts}, opts) do
          {:ok, %{count: count}} ->
            count

          {:error, error} ->
            raise Ash.Error.to_error_class(error)
        end
      end

      def count(query, opts \\ []) do
        query = Ash.Query.to_query(query)

        opts =
          if query.action do
            Keyword.put(opts, :read_action, query.action.name)
          else
            opts
          end

        {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

        case Domain.aggregate(__MODULE__, query, {:count, :count, aggregate_opts}, opts) do
          {:ok, %{count: count}} ->
            {:ok, count}

          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}
        end
      end

      def exists?(query, opts \\ []) do
        query = Ash.Query.to_query(query)

        opts =
          if query.action do
            Keyword.put(opts, :read_action, query.action.name)
          else
            opts
          end

        {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

        case Domain.aggregate(__MODULE__, query, {:exists, :exists, aggregate_opts}, opts) do
          {:ok, %{exists: exists}} ->
            exists

          {:error, error} ->
            raise Ash.Error.to_error_class(error)
        end
      end

      def exists(query, opts \\ []) do
        query = Ash.Query.to_query(query)

        opts =
          if query.action do
            Keyword.put(opts, :read_action, query.action.name)
          else
            opts
          end

        {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

        case Domain.aggregate(__MODULE__, query, {:exists, :exists, aggregate_opts}, opts) do
          {:ok, %{exists: exists}} ->
            {:ok, exists}

          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}
        end
      end

      for kind <- [:first, :sum, :list, :max, :min, :avg] do
        def unquote(kind)(query, field, opts \\ []) do
          query = Ash.Query.to_query(query)

          opts =
            if query.action do
              Keyword.put(opts, :read_action, query.action.name)
            else
              opts
            end

          {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

          case Domain.aggregate(
                 __MODULE__,
                 query,
                 {unquote(kind), unquote(kind), Keyword.put(aggregate_opts, :field, field)},
                 opts
               ) do
            {:ok, %{unquote(kind) => value}} ->
              {:ok, value}

            {:error, error} ->
              {:error, Ash.Error.to_error_class(error)}
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{kind}!")(query, field, opts \\ []) do
          query = Ash.Query.to_query(query)

          {aggregate_opts, opts} = Ash.Query.Aggregate.split_aggregate_opts(opts)

          opts =
            if query.action do
              Keyword.put(opts, :read_action, query.action.name)
            else
              opts
            end

          case Domain.aggregate(
                 __MODULE__,
                 query,
                 {unquote(kind), unquote(kind), Keyword.put(aggregate_opts, :field, field)},
                 opts
               ) do
            {:ok, %{unquote(kind) => value}} ->
              value

            {:error, error} ->
              raise Ash.Error.to_error_class(error)
          end
        end
      end

      def aggregate(query, aggregate_or_aggregates, opts \\ []) do
        case Domain.aggregate(__MODULE__, query, aggregate_or_aggregates, opts) do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def get!(resource, id_or_filter, params \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)

        Domain.get!(__MODULE__, resource, id_or_filter, params)
      end

      def get(resource, id_or_filter, params \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)
        Ash.Domain.Interface.enforce_keyword_list!(params)

        case Domain.get(__MODULE__, resource, id_or_filter, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def stream!(query, opts \\ []) do
        Ash.Domain.stream!(__MODULE__, query, opts)
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)

        Domain.read!(__MODULE__, query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)

        case Domain.read(__MODULE__, query, opts) do
          {:ok, results, query} ->
            {:ok, results, query}

          {:ok, results} ->
            {:ok, results}

          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}
        end
      end

      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)

        Domain.read_one!(__MODULE__, query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)

        case Domain.read_one(__MODULE__, query, opts) do
          {:ok, result} -> {:ok, result}
          {:ok, result, query} -> {:ok, result, query}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def page!(page, request) do
        Domain.page!(__MODULE__, page, request)
      end

      def page(page, request) do
        case Domain.page(__MODULE__, page, request) do
          {:ok, page} -> {:ok, page}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def load!(data, query, opts \\ []) do
        Domain.load!(__MODULE__, data, query, opts)
      end

      def load(data, query, opts \\ []) do
        case Domain.load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def bulk_create!(inputs, resource, action, opts \\ []) do
        Domain.bulk_create!(__MODULE__, inputs, resource, action, opts)
      end

      def bulk_create(inputs, resource, action, opts \\ []) do
        case Domain.bulk_create(__MODULE__, inputs, resource, action, opts) do
          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}

          other ->
            other
        end
      end

      def bulk_update!(stream_or_query, action, input, opts \\ []) do
        Domain.bulk_update!(__MODULE__, stream_or_query, action, input, opts)
      end

      def bulk_update(stream_or_query, action, input, opts \\ []) do
        case Domain.bulk_update(__MODULE__, stream_or_query, action, input, opts) do
          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}

          other ->
            other
        end
      end

      def bulk_destroy!(stream_or_query, action, input, opts \\ []) do
        Domain.bulk_destroy!(__MODULE__, stream_or_query, action, input, opts)
      end

      def bulk_destroy(stream_or_query, action, input, opts \\ []) do
        case Domain.bulk_destroy(__MODULE__, stream_or_query, action, input, opts) do
          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}

          other ->
            other
        end
      end

      def create!(changeset, params \\ []) do
        Domain.create!(__MODULE__, changeset, params)
      end

      def create(changeset, params \\ []) do
        case Domain.create(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def update!(changeset, params \\ []) do
        Domain.update!(__MODULE__, changeset, params)
      end

      def update(changeset, params \\ []) do
        case Domain.update(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def destroy!(record, params \\ []) do
        Domain.destroy!(__MODULE__, record, params)
      end

      def destroy(record, params \\ []) do
        case Domain.destroy(__MODULE__, record, params) do
          :ok -> :ok
          {:ok, result, notifications} -> {:ok, result, notifications}
          {:ok, notifications} -> {:ok, notifications}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        params = Keyword.put_new(params, :tenant, Map.get(record.__metadata__, :tenant))

        get!(resource, id, params)
      end

      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        params = Keyword.put_new(params, :tenant, Map.get(record.__metadata__, :tenant))
        get(resource, id, params)
      end
    end
  end

  defmacro enforce_query_or_resource!(query_or_resource) do
    quote generated: true do
      case Ash.Domain.Interface.do_enforce_query_or_resource!(unquote(query_or_resource)) do
        :ok ->
          :ok

        _ ->
          {fun, arity} = __ENV__.function
          mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"

          raise "#{mfa} expected an %Ash.Query{} or an Ash Resource but instead got #{inspect(unquote(query_or_resource))}"
      end
    end
  end

  def do_enforce_query_or_resource!(query_or_resource)
  def do_enforce_query_or_resource!(%Ash.Query{}), do: :ok

  def do_enforce_query_or_resource!(resource) when is_atom(resource) do
    if Ash.Resource.Info.resource?(resource), do: :ok, else: :error
  end

  def do_enforce_query_or_resource!(_something), do: :error

  defmacro enforce_resource!(resource) do
    quote generated: true do
      if Ash.Resource.Info.resource?(unquote(resource)) do
        :ok
      else
        {fun, arity} = __ENV__.function
        mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"

        raise Ash.Error.Invalid.NoSuchResource,
          message: "#{mfa} expected an Ash Resource but instead got #{inspect(unquote(resource))}"
      end
    end
  end

  defmacro enforce_keyword_list!(list) do
    quote generated: true do
      if Keyword.keyword?(unquote(list)) do
        :ok
      else
        {fun, arity} = __ENV__.function
        mfa = "#{inspect(__ENV__.module)}.#{fun}/#{arity}"
        raise "#{mfa} expected a keyword list, but instead got #{inspect(unquote(list))}"
      end
    end
  end
end
