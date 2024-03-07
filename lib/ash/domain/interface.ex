defmodule Ash.Domain.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote bind_quoted: [], generated: true do
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
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.can?(query_or_changeset_or_action, actor, opts)
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
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.can(action_or_query_or_changeset, actor, opts)
      end

      def run_action!(input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.run_action!(input, opts)
      end

      def run_action(input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.run_action(input, opts)
      end

      def calculate!(resource, calculation, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.calculate!(resource, calculation, opts)
      end

      def calculate(resource, calculation, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.calculate(resource, calculation, opts)
      end

      def aggregate!(query, aggregate_or_aggregates, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.aggregate!(query, aggregate_or_aggregates, opts)
      end

      def count!(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.count!(query, opts)
      end

      def count(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.count(query, opts)
      end

      def exists?(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.exists?(query, opts)
      end

      def exists(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.exists(query, opts)
      end

      for kind <- [:first, :sum, :list, :max, :min, :avg] do
        def unquote(kind)(query, field, opts \\ []) do
          opts = Keyword.put(opts, :domain, __MODULE__)
          apply(Ash, unquote(kind), [query, field, opts])
        end

        bang = :"#{kind}!"
        # sobelow_skip ["DOS.BinToAtom"]
        def unquote(bang)(query, field, opts \\ []) do
          opts = Keyword.put(opts, :domain, __MODULE__)
          apply(Ash, unquote(bang), [query, field, opts])
        end
      end

      def aggregate(query, aggregate_or_aggregates, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.aggregate(query, aggregate_or_aggregates, opts)
      end

      def get!(resource, id_or_filter, opts \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.get!(resource, id_or_filter, opts)
      end

      def get(resource, id_or_filter, opts \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.get(resource, id_or_filter, opts)
      end

      def stream!(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.stream!(query, opts)
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read!(query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read(query, opts)
      end

      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read_one!(query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read_one(query, opts)
      end

      def page!(page, request) do
        Ash.page!(page, request)
      end

      def page(page, request) do
        Ash.page(page, request)
      end

      def load!(data, query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.load!(data, query, opts)
      end

      def load(data, query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.load(data, query, opts)
      end

      def bulk_create!(inputs, resource, action, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_create!(inputs, resource, action, opts)
      end

      def bulk_create(inputs, resource, action, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_create(inputs, resource, action, opts)
      end

      def bulk_update!(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_update!(stream_or_query, action, input, opts)
      end

      def bulk_update(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_update(stream_or_query, action, input, opts)
      end

      def bulk_destroy!(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_destroy!(stream_or_query, action, input, opts)
      end

      def bulk_destroy(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_destroy(stream_or_query, action, input, opts)
      end

      def create!(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.create!(changeset, opts)
      end

      def create(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.create(changeset, opts)
      end

      def update!(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.update!(changeset, opts)
      end

      def update(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.update(changeset, opts)
      end

      def destroy!(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.destroy!(record, opts)
      end

      def destroy(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.destroy(record, opts)
      end

      def reload!(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.reload!(record, opts)
      end

      def reload(%resource{} = record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.reload(record, opts)
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
