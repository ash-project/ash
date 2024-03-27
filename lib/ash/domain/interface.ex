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
      @deprecated "Use `Ash.can?/3` instead"
      def can?(query_or_changeset_or_action, actor, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.can?(query_or_changeset_or_action, actor, opts)
      end

      @deprecated "Use `Ash.can/3` instead"
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

      @deprecated "Use `Ash.run_action/2` instead"
      def run_action!(input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.run_action!(input, opts)
      end

      @deprecated "Use `Ash.run_action/2` instead"
      def run_action(input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.run_action(input, opts)
      end

      @deprecated "Use `Ash.calculate!/3` instead"
      def calculate!(resource, calculation, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.calculate!(resource, calculation, opts)
      end

      @deprecated "Use `Ash.calculate/3` instead"
      def calculate(resource, calculation, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.calculate(resource, calculation, opts)
      end

      @deprecated "Use `Ash.count!/2` instead"
      def count!(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.count!(query, opts)
      end

      @deprecated "Use `Ash.count/2` instead"
      def count(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.count(query, opts)
      end

      @deprecated "Use `Ash.exists?/2` instead"
      def exists?(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.exists?(query, opts)
      end

      @deprecated "Use `Ash.exists/2` instead"
      def exists(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.exists(query, opts)
      end

      for kind <- [:first, :sum, :list, :max, :min, :avg] do
        @deprecated "Use `Ash.#{kind}/3` instead"
        def unquote(kind)(query, field, opts \\ []) do
          opts = Keyword.put(opts, :domain, __MODULE__)
          apply(Ash, unquote(kind), [query, field, opts])
        end

        bang = :"#{kind}!"
        # sobelow_skip ["DOS.BinToAtom"]

        @deprecated "Use `Ash.#{kind}!/3` instead"
        def unquote(bang)(query, field, opts \\ []) do
          opts = Keyword.put(opts, :domain, __MODULE__)
          apply(Ash, unquote(bang), [query, field, opts])
        end
      end

      @deprecated "Use `Ash.aggregate!/3` instead"
      def aggregate!(query, aggregate_or_aggregates, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.aggregate!(query, aggregate_or_aggregates, opts)
      end

      @deprecated "Use `Ash.aggregate/3` instead"
      def aggregate(query, aggregate_or_aggregates, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.aggregate(query, aggregate_or_aggregates, opts)
      end

      @deprecated "Use `Ash.get!/3` instead"
      def get!(resource, id_or_filter, opts \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.get!(resource, id_or_filter, opts)
      end

      @deprecated "Use `Ash.get/3` instead"
      def get(resource, id_or_filter, opts \\ []) do
        Ash.Domain.Interface.enforce_resource!(resource)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.get(resource, id_or_filter, opts)
      end

      @deprecated "Use `Ash.stream!/2` instead"
      def stream!(query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.stream!(query, opts)
      end

      @deprecated "Use `Ash.read!/2` instead"
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

      @deprecated "Use `Ash.read_one!/2` instead"
      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read_one!(query, opts)
      end

      @deprecated "Use `Ash.read_one/2` instead"
      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        Ash.Domain.Interface.enforce_query_or_resource!(query)
        Ash.Domain.Interface.enforce_keyword_list!(opts)
        opts = Keyword.put(opts, :domain, __MODULE__)

        Ash.read_one(query, opts)
      end

      @deprecated "Use `Ash.page!/2` instead"
      def page!(page, request) do
        Ash.page!(page, request)
      end

      @deprecated "Use `Ash.page/2` instead"
      def page(page, request) do
        Ash.page(page, request)
      end

      @deprecated "Use `Ash.load!/3` instead"
      def load!(data, query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.load!(data, query, opts)
      end

      @deprecated "Use `Ash.load/3` instead"
      def load(data, query, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.load(data, query, opts)
      end

      @deprecated "Use `Ash.bulk_create!/4` instead"
      def bulk_create!(inputs, resource, action, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_create!(inputs, resource, action, opts)
      end

      @deprecated "Use `Ash.bulk_create/4` instead"
      def bulk_create(inputs, resource, action, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_create(inputs, resource, action, opts)
      end

      @deprecated "Use `Ash.bulk_update!/4` instead"
      def bulk_update!(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_update!(stream_or_query, action, input, opts)
      end

      @deprecated "Use `Ash.bulk_update/4` instead"
      def bulk_update(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_update(stream_or_query, action, input, opts)
      end

      @deprecated "Use `Ash.bulk_destroy!/4` instead"
      def bulk_destroy!(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_destroy!(stream_or_query, action, input, opts)
      end

      @deprecated "Use `Ash.bulk_destroy/4` instead"
      def bulk_destroy(stream_or_query, action, input, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.bulk_destroy(stream_or_query, action, input, opts)
      end

      @deprecated "Use `Ash.create!/2` instead"
      def create!(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.create!(changeset, opts)
      end

      @deprecated "Use `Ash.create/2` instead"
      def create(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.create(changeset, opts)
      end

      @deprecated "Use `Ash.update!/2` instead"
      def update!(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.update!(changeset, opts)
      end

      @deprecated "Use `Ash.update/2` instead"
      def update(changeset, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.update(changeset, opts)
      end

      @deprecated "Use `Ash.destroy!/2` instead"
      def destroy!(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.destroy!(record, opts)
      end

      @deprecated "Use `Ash.destroy/2` instead"
      def destroy(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.destroy(record, opts)
      end

      @deprecated "Use `Ash.reload!/2` instead"
      def reload!(record, opts \\ []) do
        opts = Keyword.put(opts, :domain, __MODULE__)
        Ash.reload!(record, opts)
      end

      @deprecated "Use `Ash.reload/2` instead"
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
