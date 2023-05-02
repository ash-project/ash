defmodule Ash.Api.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
      alias Ash.Api

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
        Api.can?(__MODULE__, query_or_changeset_or_action, actor, opts)
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
        Api.can(__MODULE__, action_or_query_or_changeset, actor, opts)
      end

      def run_action!(input, opts \\ []) do
        Api.run_action!(__MODULE__, input, opts)
      end

      def run_action(input, opts \\ []) do
        Api.run_action(__MODULE__, input, opts)
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
        case Api.calculate(resource, calculation, opts) do
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

        {aggregate_opts, opts} = split_aggregate_opts(opts)

        case Ash.Query.Aggregate.new(query.resource, :count, :count, aggregate_opts) do
          {:ok, aggregate} ->
            case Api.aggregate(__MODULE__, query, aggregate, opts) do
              {:ok, %{count: count}} ->
                count

              {:error, error} ->
                raise Ash.Error.to_error_class(error)
            end

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

        {aggregate_opts, opts} = split_aggregate_opts(opts)

        case Ash.Query.Aggregate.new(query.resource, :count, :count, aggregate_opts) do
          {:ok, aggregate} ->
            case Api.aggregate(__MODULE__, query, aggregate, opts) do
              {:ok, %{count: count}} ->
                {:ok, count}

              {:error, error} ->
                {:error, Ash.Error.to_error_class(error)}
            end

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

          {aggregate_opts, opts} = split_aggregate_opts(opts)

          case Ash.Query.Aggregate.new(
                 query.resource,
                 unquote(kind),
                 unquote(kind),
                 Keyword.put(aggregate_opts, :field, field)
               ) do
            {:ok, aggregate} ->
              case Api.aggregate(__MODULE__, query, aggregate, opts) do
                {:ok, %{unquote(kind) => value}} ->
                  {:ok, value}

                {:error, error} ->
                  {:error, Ash.Error.to_error_class(error)}
              end

            {:error, error} ->
              {:error, Ash.Error.to_error_class(error)}
          end
        end

        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(:"#{kind}!")(query, field, opts \\ []) do
          query = Ash.Query.to_query(query)

          {aggregate_opts, opts} = split_aggregate_opts(opts)

          opts =
            if query.action do
              Keyword.put(opts, :read_action, query.action.name)
            else
              opts
            end

          case Ash.Query.Aggregate.new(
                 query.resource,
                 unquote(kind),
                 unquote(kind),
                 Keyword.put(aggregate_opts, :field, field)
               ) do
            {:ok, aggregate} ->
              case Api.aggregate(__MODULE__, query, aggregate, opts) do
                {:ok, %{unquote(kind) => value}} ->
                  value

                {:error, error} ->
                  raise Ash.Error.to_error_class(error)
              end

            {:error, error} ->
              raise Ash.Error.to_error_class(error)
          end
        end
      end

      defp split_aggregate_opts(opts) do
        Keyword.split(opts, Ash.Query.Aggregate.opt_keys())
      end

      def aggregate(query, aggregate_or_aggregates, opts \\ []) do
        case Api.aggregate(__MODULE__, query, aggregate_or_aggregates, opts) do
          {:ok, result} ->
            {:ok, result}

          {:error, error} ->
            {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def get!(resource, id_or_filter, params \\ []) do
        Ash.Api.Interface.enforce_resource!(resource)

        Api.get!(__MODULE__, resource, id_or_filter, params)
      end

      def get(resource, id_or_filter, params \\ []) do
        Ash.Api.Interface.enforce_resource!(resource)
        Ash.Api.Interface.enforce_keyword_list!(params)

        case Api.get(__MODULE__, resource, id_or_filter, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def stream!(query, opts \\ []) do
        Ash.Api.stream!(__MODULE__, query, opts)
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        Api.read!(__MODULE__, query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        case Api.read(__MODULE__, query, opts) do
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
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        Api.read_one!(__MODULE__, query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        Ash.Api.Interface.enforce_query_or_resource!(query)
        Ash.Api.Interface.enforce_keyword_list!(opts)

        case Api.read_one(__MODULE__, query, opts) do
          {:ok, result} -> {:ok, result}
          {:ok, result, query} -> {:ok, result, query}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def page!(page, request) do
        Api.page!(__MODULE__, page, request)
      end

      def page(page, request) do
        case Api.page(__MODULE__, page, request) do
          {:ok, page} -> {:ok, page}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def load!(data, query, opts \\ []) do
        Api.load!(__MODULE__, data, query, opts)
      end

      def load(data, query, opts \\ []) do
        case Api.load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def bulk_create!(inputs, resource, action, opts \\ []) do
        Api.bulk_create!(__MODULE__, inputs, resource, action, opts)
      end

      def bulk_create(inputs, resource, action, opts \\ []) do
        case Api.bulk_create(__MODULE__, inputs, resource, action, opts) do
          {:error, error} ->
            {:error, Ash.Error.to_error_class(error)}

          other ->
            other
        end
      end

      def create!(changeset, params \\ []) do
        Api.create!(__MODULE__, changeset, params)
      end

      def create(changeset, params \\ []) do
        case Api.create(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def update!(changeset, params \\ []) do
        Api.update!(__MODULE__, changeset, params)
      end

      def update(changeset, params \\ []) do
        case Api.update(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_error_class(error)}
        end
      end

      def destroy!(record, params \\ []) do
        Api.destroy!(__MODULE__, record, params)
      end

      def destroy(record, params \\ []) do
        case Api.destroy(__MODULE__, record, params) do
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
      case Ash.Api.Interface.do_enforce_query_or_resource!(unquote(query_or_resource)) do
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
