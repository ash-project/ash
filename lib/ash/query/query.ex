# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query do
  @moduledoc """
  A data structure for reading data from a resource.

  Queries are run by calling `Ash.read/2`.

  Examples:

  ```elixir
  require Ash.Query

  MyApp.Post
  |> Ash.Query.filter(likes > 10)
  |> Ash.Query.sort([:title])
  |> Ash.read!()

  MyApp.Author
  |> Ash.Query.aggregate(:published_post_count, :posts, query: [filter: [published: true]])
  |> Ash.Query.sort(published_post_count: :desc)
  |> Ash.Query.limit(10)
  |> Ash.read!()

  MyApp.Author
  |> Ash.Query.load([:post_count, :comment_count])
  |> Ash.Query.load(posts: [:comments])
  |> Ash.read!()
  ```

  To see more examples of what you can do with `Ash.Query` and read actions in general,
  see the [writing queries how-to guide](/documentation/how-to/write-queries.livemd).

  ## Capabilities & Limitations

  Ash Framework provides a comprehensive suite of querying tools designed to address common application development needs. While powerful and flexible, these tools are focused on domain-driven design rather than serving as a general-purpose ORM.

  Ash's query tools support:

  - Filtering records based on complex conditions
  - Sorting results using single or multiple criteria
  - Setting result limits and offsets
  - Pagination, with offset/limit and keysets
  - Selecting distinct records to eliminate duplicates
  - Computing dynamic properties at query time
  - Aggregating data from related resources

  While Ash's query tools often eliminate the need for direct database queries, Ash is not itself designed to be a comprehensive ORM or database query builder.

  For specialized querying needs that fall outside Ash's standard capabilities, the framework provides escape hatches. These mechanisms allow developers to implement custom query logic when necessary.

  ### Important Considerations

  1. Ash is primarily a domain modeling framework, not a database abstraction layer
  2. While comprehensive, the tooling is intentionally constrained to resource-oriented access
  3. Escape hatches exist for cases that require custom query logic

  For complex queries that fall outside these tools, consider whether they represent domain concepts that could be modeled differently, or if they truly require custom implementation through escape hatches.

  ## Escape Hatches

  Many of the tools in `Ash.Query` are surprisingly deep and capable, covering everything you
  need to build your domain logic. With that said, these tools are *not*
  designed to encompass *every kind of query* that you could possibly want to
  write over your data. `Ash` is *not* an ORM or a database query tool, despite
  the fact that its query building tools often make those kinds of tools
  unnecessary in all but the rarest of cases. Not every kind of query that you
  could ever wish to write can be expressed with Ash.Query. Elixir has a
  best-in-class library for working directly with databases, called
  [Ecto](https://hexdocs.pm/ecto/Ecto.html), and if you end up building a
  certain type of feature like analytics or reporting dashboards, you may find
  yourself working directly with Ecto. Data layers like AshPostgres are built
  on top of Ecto. In fact, every `Ash.Resource` is an `Ecto.Schema`!

  > ### Choose escape hatches wisely {: .warning}
  >
  > You should choose to use Ash builtin functionality wherever possible.
  > Barring that, you should choose the *least powerful* escape hatch that
  > can solve your problem. The options below are presented in the order
  > that you should prefer them, but you should only use *any of them*
  > if no builtin tooling will suffice.

  ### Fragments

  Fragments only barely count as an escape hatch. You will often find yourself
  wanting to use a function or operator specific to your data layer, and fragments
  are purpose built to this end. You can use data-layer-specific expressions in your
  expressions for filters, calculations, etc. For example:

  ```elixir
  Resource
  |> Ash.Query.filter(expr(fragment("lower(?)", name) == "fred"))
  |> Ash.Query.filter(expr(fragment("? @> ?", tags, ["important"])))
  ```

  ### Manual Read Actions

  See [the manual read actions guide](/documentation/topics/actions/manual-actions.md).

  ### `d:Ash.Resource.Dsl|actions.read.modify_query`

  When running read actions, you can modify the underlying data layer query directly,
  which can solve for cases when you cannot express your query using the standard Ash query interface.

  ```elixir
  actions do
    read :complex_search do
      argument
      modify_query {SearchMod, :modify, []}
    end
  end
  ```

  ```elixir
  defmodule SearchMod do
    def modify(ash_query, data_layer_query) do
      # Here you can modify the underlying data layer query directly
      # For example, with AshPostgres you get access to the Ecto query
      {:ok, Ecto.Query.where(data_layer_query, [p], fragment("? @@ plainto_tsquery(?)", p.search_vector, ^ash_query.arguments.search_text))}
    end
  end
  ```

  ### Using Ecto directly

  For data layers like `AshPostgres`, you can interact directly with `Ecto`. You can do this
  by using the `Ash.Resource` as its corresponding `Ecto.Schema`, like so:

  ```elixir
  import Ecto.Query

  query =
    from p in MyApp.Post,
      where: p.likes > 100,
      select: p

   MyApp.Repo.all(query)
  ```

  Or you can build an `Ash.Query`, and get the corresponding ecto query:

  ```elixir
  MyApp.Post
  |> Ash.Query.for_read(:read)
  |> Ash.data_layer_query()
  |> case do
    {:ok, %{query: ecto_query}} ->
      ecto_query
      |> Ecto.Query.where([p], p.likes > 100)
      |> MyApp.Repo.all()

    {:error, error} ->
      {:error, error}
  end
  ```
  """

  defstruct [
    :__validated_for_action__,
    :action,
    :domain,
    :distinct,
    :filter,
    :resource,
    :tenant,
    :timeout,
    :lock,
    :to_tenant,
    sort_input_indices: [],
    before_transaction: [],
    after_transaction: [],
    around_transaction: [],
    invalid_keys: MapSet.new(),
    load_through: %{},
    action_failed?: false,
    combination_of: [],
    after_action: [],
    authorize_results: [],
    aggregates: %{},
    arguments: %{},
    before_action: [],
    calculations: %{},
    context: %{},
    errors: [],
    limit: nil,
    load: [],
    offset: 0,
    page: nil,
    params: %{},
    phase: :preparing,
    select: nil,
    sort: [],
    distinct_sort: [],
    valid?: true
  ]

  @typedoc """
  A query struct for reading data from a resource.

  Contains all the configuration needed to read data including filters, sorting,
  pagination, field selection, and relationship loading. Built incrementally
  through functions like `filter/2`, `sort/2`, `load/2`, etc.
  """
  @type t :: %__MODULE__{
          __validated_for_action__: atom | nil,
          action: Ash.Resource.Actions.Read.t() | nil,
          domain: module | nil,
          distinct: [atom],
          filter: Ash.Filter.t() | nil,
          resource: module,
          tenant: term(),
          combination_of: [Ash.Query.Combination.t()],
          timeout: pos_integer() | nil,
          action_failed?: boolean,
          after_action: [
            (t, [Ash.Resource.record()] ->
               {:ok, [Ash.Resource.record()]}
               | {:ok, [Ash.Resource.record()], [Ash.Notifier.Notification.t()]}
               | {:error, any})
          ],
          authorize_results: [
            (t, [Ash.Resource.record()] ->
               {:ok, [Ash.Resource.record()]}
               | {:error, any})
          ],
          aggregates: %{optional(atom) => Ash.Filter.t()},
          arguments: %{optional(atom) => any},
          before_action: [(t -> t)],
          before_transaction: [before_transaction_fun],
          after_transaction: [after_transaction_fun],
          around_transaction: [around_transaction_fun],
          calculations: %{optional(atom) => :wat},
          context: map,
          errors: [Ash.Error.t()],
          limit: nil | non_neg_integer(),
          load: keyword(keyword),
          offset: non_neg_integer(),
          page: keyword() | nil | false,
          params: %{optional(atom | binary) => any},
          phase: :preparing | :before_action | :after_action | :executing,
          select: nil | [atom],
          sort: [atom | {atom, :asc | :desc}],
          valid?: boolean
        }

  @read_action_after_action_hooks_in_order? Application.compile_env(
                                              :ash,
                                              :read_action_after_action_hooks_in_order?,
                                              false
                                            )

  @typedoc "Result type for around_transaction hooks, containing either successful records or an error."
  @type around_result ::
          {:ok, list(Ash.Resource.record())}
          | {:error, Ash.Error.t()}

  @typedoc "Function type for before_transaction hooks that run before query execution."
  @type before_transaction_fun :: (t -> t | {:error, any})

  @typedoc "Function type for after_transaction hooks that run after query execution."
  @type after_transaction_fun ::
          (t, {:ok, list(Ash.Resource.record())} | {:error, any} ->
             {:ok, list(Ash.Resource.record())} | {:error, any})

  @typedoc "Function type for around_transaction hooks that wrap query execution in a transaction."
  @type around_transaction_fun :: (t -> {:ok, Ash.Resource.record()} | {:error, any})

  alias Ash.Actions.Sort

  alias Ash.Error.Invalid.TimeoutNotSupported
  alias Ash.Error.Load.NoSuchRelationship

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidArgument,
    InvalidCalculationArgument,
    InvalidLimit,
    InvalidOffset,
    InvalidPage,
    InvalidQuery,
    NoReadAction,
    ReadActionRequiresActor,
    Required
  }

  alias Ash.Query.{Aggregate, Calculation}

  require Ash.Tracer

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(query, opts) do
      load_through_attributes = Map.to_list(query.load_through[:attributes] || %{})

      query = %{
        query
        | load: Keyword.merge(query.load || [], load_through_attributes),
          calculations:
            Map.new(query.calculations, fn {name, calc} ->
              if load_through = query.load_through[:calculations][name] do
                {name, {calc, load_through}}
              else
                {name, calc}
              end
            end)
      }

      query = %{
        query
        | calculations: Ash.Actions.Read.Calculations.map_without_calc_deps(query.calculations)
      }

      sort? = query.sort != []
      distinct_sort? = query.distinct_sort != []
      load? = query.load != []
      aggregates? = query.aggregates != %{}
      calculations? = query.calculations != %{}
      limit? = not is_nil(query.limit)
      offset? = not (is_nil(query.offset) || query.offset == 0)
      filter? = not is_nil(query.filter)
      errors? = not Enum.empty?(query.errors)
      tenant? = not is_nil(query.tenant)
      select? = query.select not in [[], nil]
      distinct? = query.distinct not in [[], nil]
      lock? = not is_nil(query.lock)
      page? = not is_nil(query.page)
      combination_of? = query.combination_of != []

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", inspect(query.resource)),
          or_empty(
            concat("action: ", inspect(query.action && query.action.name)),
            not is_nil(query.action)
          ),
          or_empty(concat("tenant: ", to_doc(query.to_tenant, opts)), tenant?),
          arguments(query, opts),
          # TODO: inspect these specially
          or_empty(
            concat(
              "combination_of: ",
              to_doc(query.combination_of, %{opts | custom_options: [in_query?: true]})
            ),
            combination_of?
          ),
          or_empty(concat("filter: ", to_doc(query.filter, opts)), filter?),
          or_empty(concat("sort: ", to_doc(query.sort, opts)), sort?),
          or_empty(concat("distinct_sort: ", to_doc(query.distinct_sort, opts)), distinct_sort?),
          or_empty(concat("limit: ", to_doc(query.limit, opts)), limit?),
          or_empty(concat("offset: ", to_doc(query.offset, opts)), offset?),
          or_empty(concat("load: ", to_doc(query.load, opts)), load?),
          or_empty(concat("aggregates: ", to_doc(query.aggregates, opts)), aggregates?),
          or_empty(concat("calculations: ", to_doc(query.calculations, opts)), calculations?),
          or_empty(concat("errors: ", to_doc(query.errors, opts)), errors?),
          or_empty(concat("select: ", to_doc(query.select, opts)), select?),
          or_empty(concat("distinct: ", to_doc(query.distinct, opts)), distinct?),
          or_empty(concat("lock: ", to_doc(query.lock, opts)), lock?),
          or_empty(concat("page: ", to_doc(query.page, opts)), page?)
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end

    defp arguments(query, opts) do
      if query.action do
        if is_nil(query.action) || Enum.empty?(query.action.arguments) do
          empty()
        else
          args =
            query.action.arguments
            |> Enum.reduce(%{}, fn argument, acc ->
              case Ash.Query.fetch_argument(query, argument.name) do
                {:ok, value} ->
                  if argument.sensitive? do
                    Map.put(acc, argument.name, "**redacted**")
                  else
                    Map.put(acc, argument.name, value)
                  end

                :error ->
                  acc
              end
            end)

          if args == %{} do
            empty()
          else
            concat(["arguments: ", to_doc(args, opts)])
          end
        end
      else
        empty()
      end
    end

    defp or_empty(value, true), do: value
    defp or_empty(_, false), do: empty()
  end

  defmacrop maybe_already_validated_error!(query) do
    {function, _arity} = __CALLER__.function

    quote do
      query = unquote(query)

      if !is_atom(query) && query.__validated_for_action__ do
        {:current_stacktrace, stacktrace} =
          Process.info(self(), :current_stacktrace)

        require Logger

        Logger.warning("""
        Query has already been validated for action #{inspect(query.__validated_for_action__)}.

        For safety, we prevent any changes after that point because they will bypass validations or other action logic.
        However, you should prefer a pattern like the below, which makes any custom modifications *before* calling the action.

          Resource
          |> Ash.Query.new()
          |> Ash.Query.#{unquote(function)}(...)
          |> Ash.Query.for_read(...)

        #{Exception.format_stacktrace(stacktrace)}
        """)
      end
    end
  end

  @doc """
  Attach a filter statement to the query labelled as user input.

  Filters added as user input (or filters constructed with `Ash.Filter.parse_input`)
  will honor any field policies on resources by replacing any references to the field
  with `nil` in cases where the actor should not be able to see the given field.

  This function does not expect the expression style filter (because an external source
  could never reasonably provide that). Instead, use the keyword/map style syntax. For
  example:

  `expr(name == "fred")`

  could be any of

  - map syntax: `%{"name" => %{"eq" => "fred"}}`
  - keyword syntax: `[name: [eq: "fred"]]`

  See `Ash.Filter` for more.
  """
  def filter_input(query, filter) do
    query = new(query)

    case Ash.Filter.parse_input(query.resource, filter) do
      {:ok, filter} ->
        do_filter(query, filter)

      {:error, error} ->
        add_error(query, :filter, error)
    end
  end

  @doc """
  Produces a query that is the combination of multiple queries.

  All aspects of the parent query are applied to the combination in total.

  See `Ash.Query.Combination` for more on creating combination queries.

  ### Example

  ```elixir
  # Top ten users not on a losing streak and top ten users who are not on a winning streak
  User
  |> Ash.Query.filter(active == true)
  |> Ash.Query.combination_of([
    # must always begin with a base combination
    Ash.Query.Combination.base(
      sort: [score: :desc],
      filter: expr(not(on_a_losing_streak)),
      limit: 10
    ),
    Ash.Query.Combination.union(
      sort: [score: :asc],
      filter: expr(not(on_a_winning_streak)),
      limit: 10
    )
  ])
  |> Ash.read!()
  ```

  ### Select and calculations

  There is no `select` available for combinations, instead the select of the outer query
  is used for each combination. However, you can use the `calculations` field in
  `Ash.Query.Combination` to add expression calculations. Those calculations can "overwrite"
  a selected attribute, or can introduce a new field. Note that, for SQL data layers, all
  combinations will be required to have the same number of fields in their SELECT statement,
  which means that if one combination adds a calculation, all of the others must also add
  that calculation.

  In this example, we compute separate match scores

  ```elixir
  query = "fred"

  User
  |> Ash.Query.filter(active == true)
  |> Ash.Query.combination_of([
    # must always begin with a base combination
    Ash.Query.Combination.base(
      filter: expr(trigram_similarity(user_name, ^query) >= 0.5),
      calculate: %{
        match_score: trigram_similarity(user_name, ^query)
      },
      sort: [
        calc(trigram_similarity(user_name, ^query), :desc)
      ],
      limit: 10
    ),
    Ash.Query.Combination.union(
      filter: expr(trigram_similarity(email, ^query) >= 0.5),
      calculate: %{
        match_score: trigram_similarity(email, ^query)
      },
      sort: [
        calc(trigram_similarity(email, ^query), :desc)
      ],
      limit: 10
    )
  ])
  |> Ash.read!()
  ```
  """
  @spec combination_of(t(), Ash.Query.Combination.t() | [Ash.Query.Combination.t()]) :: t()
  def combination_of(query, combinations) do
    query = new(query)

    %{query | combination_of: query.combination_of ++ List.wrap(combinations)}
  end

  @doc """
  Attach a sort statement to the query labelled as user input.

  Sorts added as user input (or filters constructed with `Ash.Filter.parse_input`)
  will honor any field policies on resources by replacing any references to the field
  with `nil` in cases where the actor should not be able to see the given field.

  See `Ash.Query.sort/3` for more information on accepted formats.
  """
  def sort_input(query, sorts, opts \\ []) do
    query = new(query)

    if sorts == [] || sorts == nil do
      query
    else
      if Ash.DataLayer.data_layer_can?(query.resource, :sort) do
        if opts[:prepend?] && query.sort != [] do
          validated =
            query
            |> Map.put(:sort, [])
            |> sort_input(sorts)
            |> Map.get(:sort)

          new_sort_input_indices =
            Enum.to_list(0..(Enum.count(List.wrap(validated)) - 1)) ++
              Enum.map(query.sort_input_indices, &(&1 + 1))

          %{query | sort: validated ++ query.sort, sort_input_indices: new_sort_input_indices}
        else
          last_index = Enum.count(List.wrap(query.sort))

          case Ash.Sort.parse_input(query.resource, sorts) do
            {:ok, sorts} ->
              sorts
              |> List.wrap()
              |> Enum.with_index(last_index)
              |> Enum.reduce(query, fn
                {{sort, direction}, index}, query ->
                  %{
                    query
                    | sort: query.sort ++ [{sort, direction}],
                      sort_input_indices: query.sort_input_indices ++ [index]
                  }

                {sort, index}, query ->
                  %{
                    query
                    | sort: query.sort ++ [{sort, :asc}],
                      sort_input_indices: query.sort_input_indices ++ [index]
                  }
              end)

            {:error, error} ->
              Ash.Query.add_error(query, :sort, error)
          end
        end
      else
        add_error(query, :sort, "Data layer does not support sorting")
      end
    end
    |> sequence_sorts()
  end

  defp sequence_sorts(query) do
    %{
      query
      | sort: sequence_sort(query.sort),
        distinct_sort: sequence_sort(query.distinct_sort),
        distinct: sequence_sort(query.distinct)
    }
  end

  defp sequence_sort(nil), do: nil

  # sobelow_skip ["DOS.BinToAtom", "DOS.StringToAtom"]
  defp sequence_sort(statement) do
    statement
    |> Enum.with_index()
    |> Enum.map(fn
      {{%Ash.Query.Calculation{name: :__calc__} = field, direction}, index} ->
        {%{field | name: String.to_atom("__calc__#{index}"), load: nil}, direction}

      {other, _} ->
        other
    end)
  end

  @doc """
  Attach a filter statement to the query.

  The filter is applied as an "and" to any filters currently on the query.
  Filters allow you to specify conditions that records must meet to be included
  in the query results. Multiple filters on the same query are combined with "and" logic.

  ## Examples

      # Filter with simple equality
      MyApp.Post
      |> Ash.Query.filter(published: true)

      # Filter with comparison operators
      MyApp.Post
      |> Ash.Query.filter(view_count > 100)

      # Filter with complex expressions using do block
      MyApp.Post
      |> Ash.Query.filter do
        published == true and view_count > 100
      end

  ## See also

  - `Ash.Filter` for comprehensive filter documentation
  - `sort/3` for ordering query results
  - `Ash.read/2` for executing filtered queries
  """
  defmacro filter(query, %Ash.Filter{} = filter) do
    quote location: :keep do
      Ash.Query.do_filter(unquote(query), unquote(filter))
    end
  end

  defmacro filter(query, nil), do: query
  defmacro filter(query, true), do: query

  defmacro filter(query, false) do
    quote location: :keep do
      Ash.Query.do_filter(unquote(query), false)
    end
  end

  defmacro filter(query, do: body) do
    quote location: :keep do
      Ash.Query.do_filter(unquote(query), unquote(body))
    end
  end

  defmacro filter(query, expression) do
    if Keyword.keyword?(expression) do
      quote location: :keep do
        Ash.Query.do_filter(unquote(query), unquote(expression))
      end
    else
      quote location: :keep do
        require Ash.Expr
        expr = Ash.Expr.expr(unquote(expression))
        Ash.Query.do_filter(unquote(query), expr)
      end
    end
  end

  @doc """
  Creates a new query for the given resource.

  This is the starting point for building queries.  The query will automatically include the resource's base filter
  and default context.

  ## Examples

      # Create a new query for a resource
      iex> Ash.Query.new(MyApp.Post)
      %Ash.Query{resource: MyApp.Post, ...}

      # Create a query with options
      iex> Ash.Query.new(MyApp.Post, domain: MyApp.Blog)
      %Ash.Query{resource: MyApp.Post, domain: MyApp.Blog, ...}

      # Pass an existing query (returns the query unchanged)
      iex> query = Ash.Query.new(MyApp.Post)
      iex> Ash.Query.new(query)
      %Ash.Query{resource: MyApp.Post, ...}

  ## See also

  - `for_read/4` for creating queries bound to specific read actions
  - `filter/2` for adding filter conditions
  - `sort/3` for adding sort criteria
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
  """
  @spec new(Ash.Resource.t() | Ash.Query.t(), opts :: Keyword.t()) :: Ash.Query.t()
  def new(resource, opts \\ [])
  def new(%__MODULE__{} = query, _opts), do: query

  def new(resource, opts) when is_atom(resource) do
    if !Ash.Resource.Info.resource?(resource) do
      raise ArgumentError,
            "Expected a resource or a query in `Ash.Query.new/2`, got: `#{inspect(resource)}`"
    end

    query = %__MODULE__{
      domain: opts[:domain],
      filter: nil,
      resource: resource
    }

    query =
      case Ash.Resource.Info.base_filter(resource) do
        nil ->
          query

        filter ->
          if Keyword.get(opts, :base_filter?, true) do
            filter =
              resource
              |> Ash.Filter.parse!(filter, query.context)
              |> Ash.Filter.embed_predicates()

            do_filter(query, filter)
          else
            query
          end
      end

    case Ash.Resource.Info.default_context(resource) do
      nil ->
        query

      context ->
        Ash.Query.set_context(query, context)
    end

    context = Process.get(:ash_context, %{}) || %{}

    query
    |> set_context(context)
    |> Ash.DataLayer.transform_query()
  end

  def new(resource, _) do
    raise ArgumentError,
          "Expected a resource or a query in `Ash.Query.new/2`, got: `#{inspect(resource)}`"
  end

  @for_read_opts [
    actor: [
      type: :any,
      doc:
        "set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol, for passing around actor/tenant/context in a single value. See `Ash.Scope.ToOpts` for more."
    ],
    authorize?: [
      type: :boolean,
      doc:
        "set authorize?, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    tracer: [
      type: {:wrap_list, {:behaviour, Ash.Tracer}},
      doc:
        "A tracer to use. Will be carried over to the action. For more information see `Ash.Tracer`."
    ],
    tenant: [
      type: {:protocol, Ash.ToTenant},
      doc: "set the tenant on the query"
    ],
    load: [
      type: :any,
      doc: "A load statement to apply to the query"
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc:
        "A list of inputs that, if provided, will be ignored if they are not recognized by the action. Use `:*` to indicate all unknown keys."
    ],
    context: [
      type: :map,
      doc:
        "A map of context to set on the query. This will be merged with any context set on the query itself."
    ]
  ]

  @doc false
  def for_read_opts, do: @for_read_opts

  @doc """
  Creates a query for a given read action and prepares it.

  This function configures the query to use a specific read action with the provided
  arguments and options. The query will be validated and prepared according to the
  action's configuration, including applying preparations and action filters.

  Multitenancy is *not* validated until an action is called. This allows you to avoid
  specifying a tenant until just before calling the domain action.

  ## Examples

      # Create a query for a simple read action
      iex> Ash.Query.for_read(MyApp.Post, :read)
      %Ash.Query{action: %{name: :read}, ...}

      # Create a query with arguments for a parameterized action
      iex> Ash.Query.for_read(MyApp.Post, :published, %{since: ~D[2023-01-01]})
      %Ash.Query{action: %{name: :published}, arguments: %{since: ~D[2023-01-01]}, ...}

      # Create a query with options
      iex> Ash.Query.for_read(MyApp.Post, :read, %{}, actor: current_user, authorize?: true)
      %Ash.Query{action: %{name: :read}, ...}

  ## Options

  #{Spark.Options.docs(@for_read_opts)}

  ## See also

  - `Ash.read/2` for executing the prepared query
  - `new/2` for creating basic queries without specific actions
  - `load/3` for adding relationship loading to queries
  - `d:Ash.Resource.Dsl.actions.read` for defining read actions
  - [Read Actions Guide](/documentation/topics/actions/read-actions.md) for understanding read operations
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
  """
  @spec for_read(t() | Ash.Resource.t(), atom(), map() | Keyword.t(), Keyword.t()) :: t()
  # 4.0: make args required, same with action input and changeset
  def for_read(query, action_name, args \\ %{}, opts \\ []) do
    query = new(query)

    domain =
      query.domain || opts[:domain] || Ash.Resource.Info.domain(query.resource) ||
        Ash.Actions.Helpers.maybe_embedded_domain(query.resource) ||
        raise ArgumentError,
              "Could not determine domain for query. Provide the `domain` option or configure a domain in the resource directly."

    {query, opts} =
      Ash.Actions.Helpers.set_context_and_get_opts(
        domain,
        query,
        opts
      )

    query =
      query
      |> Map.put(:params, Map.merge(query.params, Map.new(args)))
      |> set_context(Keyword.get(opts, :context, %{}))

    action = Ash.Resource.Info.action(query.resource, action_name, :read)

    if action do
      name = fn ->
        "query:" <> Ash.Resource.Info.trace_name(query.resource) <> ":#{action_name}"
      end

      query =
        if opts[:load] do
          load(query, opts[:load])
        else
          query
        end

      Ash.Tracer.span :query,
                      name,
                      opts[:tracer] do
        Ash.Tracer.telemetry_span [:ash, :query], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(query.resource)
          }
        end do
          metadata = fn ->
            %{
              resource_short_name: Ash.Resource.Info.short_name(query.resource),
              resource: query.resource,
              actor: opts[:actor],
              tenant: opts[:tenant],
              action: action.name,
              authorize?: opts[:authorize?]
            }
          end

          Ash.Tracer.set_metadata(opts[:tracer], :query, metadata)

          query
          |> Map.put(:action, action)
          |> reset_arguments()
          |> timeout(query.timeout || opts[:timeout])
          |> set_actor(opts)
          |> set_authorize?(opts)
          |> set_tracer(opts)
          |> set_tenant(opts[:tenant] || query.tenant)
          |> cast_params(action, args, opts)
          |> set_argument_defaults(action)
          |> require_arguments(action)
          |> run_preparations(action, opts[:actor], opts[:authorize?], opts[:tracer], metadata)
          |> add_action_filters(action, opts[:actor])
          |> Map.put(:__validated_for_action__, action_name)
        end
      end
    else
      raise_no_action(query.resource, action_name)
    end
  end

  @doc """
  Set a timeout for the query.

  For more information, see the [timeouts guide](/documentation/topics/advanced/timeouts.md)
  """
  @spec timeout(t(), pos_integer() | :infinity | nil) :: t()
  def timeout(query, timeout) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :timeout) || is_nil(timeout) do
      %{query | timeout: timeout}
    else
      add_error(query, TimeoutNotSupported.exception(resource: query.resource))
    end
  end

  @doc false
  def set_actor(query, opts) do
    if Keyword.has_key?(opts, :actor) do
      put_context(query, :private, %{actor: opts[:actor]})
    else
      query
    end
  end

  @doc false
  def set_authorize?(query, opts) do
    if Keyword.has_key?(opts, :authorize?) do
      put_context(query, :private, %{authorize?: opts[:authorize?]})
    else
      query
    end
  end

  @doc false
  def set_tracer(query, opts) do
    if Keyword.has_key?(opts, :tracer) do
      put_context(query, :private, %{tracer: opts[:tracer]})
    else
      query
    end
  end

  defp raise_no_action(resource, action_name) do
    available_actions =
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.filter(&(&1.type == :read))
      |> Enum.map_join("\n", &"    - `#{inspect(&1.name)}")

    raise ArgumentError,
      message: """
      No such read action on resource #{inspect(resource)}: #{String.slice(inspect(action_name), 0..50)}

      Example Call:

        Ash.Query.for_read(query_or_resource, :action_name, input, options)

      Available read actions:

      #{available_actions}
      """
  end

  defp require_arguments(query, action) do
    action.arguments
    |> Enum.filter(&(&1.allow_nil? == false))
    |> Enum.reduce(query, fn argument, query ->
      case fetch_argument(query, argument.name) do
        {:ok, value} when not is_nil(value) ->
          query

        _ ->
          if argument.name in query.invalid_keys do
            query
          else
            add_error(
              query,
              Required.exception(
                resource: query.resource,
                field: argument.name,
                type: :argument
              )
            )
          end
      end
    end)
  end

  defp set_argument_defaults(query, action) do
    Enum.reduce(action.arguments, query, fn argument, query ->
      case fetch_argument(query, argument.name) do
        :error ->
          if is_nil(argument.default) do
            query
          else
            %{
              query
              | arguments:
                  Map.put(query.arguments, argument.name, argument_default(argument.default))
            }
          end

        _ ->
          query
      end
    end)
  end

  defp cast_params(query, action, args, opts) do
    skip_unknown_inputs = List.wrap(opts[:skip_unknown_inputs] || action.skip_unknown_inputs)

    Enum.reduce(args, query, fn {name, value}, query ->
      cond do
        has_argument?(action, name) ->
          set_argument(query, name, value)

        :* in skip_unknown_inputs ->
          query

        name in skip_unknown_inputs ->
          query

        match?("_" <> _, name) ->
          query

        true ->
          error =
            Ash.Error.Invalid.NoSuchInput.exception(
              resource: query.resource,
              action: query.action.name,
              input: name,
              inputs: Enum.map(query.action.arguments, & &1.name)
            )

          add_error(query, Ash.Error.set_path(error, name))
      end
    end)
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.public? && &1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(&1.public? && to_string(&1.name) == name))
  end

  defp has_key?(map, key) when is_map(map), do: Map.has_key?(map, key)
  defp has_key?(keyword, key), do: Keyword.has_key?(keyword, key)

  defp run_preparations(query, action, actor, authorize?, tracer, metadata) do
    global_validations =
      if action.skip_global_validations? do
        []
      else
        Ash.Resource.Info.validations(query.resource, :read)
      end

    query.resource
    # 4.0 make global preparations happen
    # after action level preparations
    |> Ash.Resource.Info.preparations()
    |> Enum.concat(action.preparations || [])
    |> Enum.concat(global_validations)
    |> Enum.reduce(query, fn
      %{only_when_valid?: true}, %{valid?: false} = query ->
        query

      %{validation: {module, opts}} = validation, query ->
        if __MODULE__ not in module.supports(opts) do
          raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
        end

        validate(query, validation, tracer, metadata, actor)

      %{preparation: _} = preparation, query ->
        run_preparation(preparation, query, actor, authorize?, tracer, metadata)
    end)
  end

  defp validate(query, validation, tracer, metadata, actor) do
    if validation.before_action? do
      before_action(query, fn query ->
        if validation.only_when_valid? and not query.valid? do
          query
        else
          do_validation(query, validation, tracer, metadata, actor)
        end
      end)
    else
      if validation.only_when_valid? and not query.valid? do
        query
      else
        do_validation(query, validation, tracer, metadata, actor)
      end
    end
  end

  defp do_validation(query, validation, tracer, metadata, actor) do
    context = %{
      actor: query.context[:private][:actor],
      tenant: query.tenant,
      source_context: query.context,
      authorize?: query.context[:private][:authorize?] || false,
      tracer: query.context[:private][:tracer]
    }

    if Enum.all?(validation.where || [], fn {module, opts} ->
         if __MODULE__ not in module.supports(opts) do
           raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
         end

         opts =
           Ash.Expr.fill_template(
             opts,
             actor: actor,
             tenant: query.to_tenant,
             args: query.arguments,
             context: query.context
           )

         case module.init(opts) do
           {:ok, opts} ->
             Ash.Resource.Validation.validate(
               module,
               query,
               opts,
               struct(Ash.Resource.Validation.Context, context)
             ) ==
               :ok

           _ ->
             false
         end
       end) do
      Ash.Tracer.span :validation, fn -> "validate: #{inspect(validation.module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :validation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            validation: inspect(validation.module)
          }
        end do
          Ash.Tracer.set_metadata(tracer, :validation, metadata)

          opts =
            Ash.Expr.fill_template(
              validation.opts,
              actor: actor,
              tenant: query.to_tenant,
              args: query.arguments,
              context: query.context
            )

          with {:ok, opts} <- validation.module.init(opts),
               :ok <-
                 Ash.Resource.Validation.validate(
                   validation.module,
                   query,
                   opts,
                   struct(
                     Ash.Resource.Validation.Context,
                     Map.put(context, :message, validation.message)
                   )
                 ) do
            query
          else
            :ok ->
              query

            {:error, error} when is_binary(error) ->
              add_error(query, validation.message || error)

            {:error, error} when is_exception(error) ->
              if validation.message do
                error = Ash.Error.override_validation_message(error, validation.message)
                add_error(query, error)
              else
                add_error(query, error)
              end

            {:error, errors} when is_list(errors) ->
              if validation.message do
                errors =
                  Enum.map(errors, fn error ->
                    Ash.Error.override_validation_message(error, validation.message)
                  end)

                add_error(query, errors)
              else
                add_error(query, errors)
              end

            {:error, error} ->
              error =
                if Keyword.keyword?(error) do
                  Keyword.put(error, :message, validation.message || error[:message])
                else
                  validation.message || error
                end

              add_error(query, error)
          end
        end
      end
    else
      query
    end
  end

  defp run_preparation(
         %{preparation: {module, opts}} = preparation,
         query,
         actor,
         authorize?,
         tracer,
         metadata
       ) do
    context = %{
      actor: actor,
      tenant: query.tenant,
      source_context: query.context,
      authorize?: authorize? || false,
      tracer: tracer
    }

    if Enum.all?(preparation.where || [], fn {module, opts} ->
         if __MODULE__ not in module.supports(opts) do
           raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
         end

         opts =
           Ash.Expr.fill_template(
             opts,
             actor: actor,
             tenant: query.to_tenant,
             args: query.arguments,
             context: query.context
           )

         case module.init(opts) do
           {:ok, opts} ->
             Ash.Resource.Validation.validate(
               module,
               query,
               opts,
               struct(Ash.Resource.Validation.Context, context)
             ) ==
               :ok

           _ ->
             false
         end
       end) do
      Ash.Tracer.span :preparation, fn -> "prepare: #{inspect(module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :preparation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            preparation: inspect(module)
          }
        end do
          Ash.Tracer.set_metadata(tracer, :preparation, metadata)

          {:ok, opts} = module.init(opts)

          opts =
            Ash.Expr.fill_template(
              opts,
              actor: actor,
              tenant: query.to_tenant,
              args: query.arguments,
              context: query.context
            )

          preparation_context = struct(Ash.Resource.Preparation.Context, context)
          Ash.Resource.Preparation.prepare(module, query, opts, preparation_context)
        end
      end
    else
      query
    end
  end

  @doc """
  Adds a before_transaction hook to the query.

  The before_transaction hook runs before the database transaction begins.
  It receives the query and must return either a modified query or an error.

  ## Examples

      # Add logging before transaction
      iex> query = MyApp.Post |> Ash.Query.before_transaction(fn query ->
      ...>   IO.puts("Starting transaction for \#{inspect(query.resource)}")
      ...>   query
      ...> end)

  ## See also

  - `after_transaction/2` for hooks that run after the transaction
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `before_action/3` for hooks that run before the action (inside transaction)
  """
  @spec before_transaction(
          query :: t(),
          fun :: before_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def before_transaction(query, func, opts \\ []) do
    query = new(query)

    if opts[:prepend?] do
      %{query | before_transaction: [func | query.before_transaction]}
    else
      %{query | before_transaction: query.before_transaction ++ [func]}
    end
  end

  @doc """
  Adds an after_transaction hook to the query.

  The after_transaction hook runs after the database transaction completes,
  regardless of success or failure. It receives the query and the result,
  and can modify the result or perform cleanup operations.

  ## Examples

      # Add cleanup after transaction
      iex> query = MyApp.Post |> Ash.Query.after_transaction(fn query, result ->
      ...>   cleanup_resources()
      ...>   result
      ...> end)

  ## See also

  - `before_transaction/2` for hooks that run before the transaction
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `after_action/2` for hooks that run after the action (inside transaction)
  """
  @spec after_transaction(
          query :: t(),
          fun :: after_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def after_transaction(query, func, opts \\ []) do
    query = new(query)

    if opts[:prepend?] do
      %{query | after_transaction: [func | query.after_transaction]}
    else
      %{query | after_transaction: query.after_transaction ++ [func]}
    end
  end

  @doc """
  Adds an around_transaction hook to the query.

  Your function will get the query, and a callback that must be called with a query (that may be modified).
  The callback will return `{:ok, results}` or `{:error, error}`. You can modify these values, but the return value
  must be one of those types.

  The around_transaction calls happen first, and then (after they each resolve their callbacks) the `before_action`
  hooks are called, followed by the `after_action` hooks being run. Then, the code that appeared *after* the callbacks were called is then run.

  ## Examples

      # Add logging around the transaction
      iex> query = MyApp.Post |> Ash.Query.around_transaction(fn query, callback ->
      ...>   IO.puts("Starting transaction for \#{inspect(query.resource)}")
      ...>   result = callback.(query)
      ...>   IO.puts("Transaction completed: \#{inspect(result)}")
      ...>   result
      ...> end)

      # Add error handling and retry logic
      iex> query = MyApp.Post |> Ash.Query.around_transaction(fn query, callback ->
      ...>   case callback.(query) do
      ...>     {:ok, results} = success -> success
      ...>     {:error, %{retryable?: true}} ->
      ...>       callback.(query)  # Retry once
      ...>     error -> error
      ...>   end
      ...> end)

  ## Warning

  Using this without understanding how it works can cause big problems.
  You *must* call the callback function that is provided to your hook, and the return value must
  contain the same structure that was given to you, i.e `{:ok, result_of_action}`.

  ## See also

  - `before_transaction/2` for hooks that run before the transaction
  - `after_transaction/2` for hooks that run after the transaction
  - `before_action/3` for hooks that run before the action executes
  - `after_action/2` for hooks that run after the action completes
  - `Ash.read/2` for executing queries with hooks
  """

  @spec around_transaction(
          query :: t(),
          fun :: around_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def around_transaction(query, func, opts \\ []) do
    query = new(query)

    if opts[:prepend?] do
      %{query | around_transaction: [func | query.around_transaction]}
    else
      %{query | around_transaction: query.around_transaction ++ [func]}
    end
  end

  @doc """
  Adds a before_action hook to the query.

  Before action hooks are called after preparations but before the actual
  data layer query is executed. They receive the prepared query and can
  modify it or perform side effects before the action runs.

  ## Examples

      # Add validation before the query runs
      iex> query = MyApp.Post
      ...> |> Ash.Query.before_action(fn query ->
      ...>   if Enum.empty?(query.sort) do
      ...>     Ash.Query.sort(query, :created_at)
      ...>   else
      ...>     query
      ...>   end
      ...> end)

      # Add logging before the action
      iex> query = MyApp.Post
      ...> |> Ash.Query.before_action(fn query ->
      ...>   IO.puts("Executing query for \#{length(query.filter || [])} filters")
      ...>   query
      ...> end)

      # Prepend a hook to run first
      iex> query = MyApp.Post
      ...> |> Ash.Query.before_action(&setup_query/1)
      ...> |> Ash.Query.before_action(&early_validation/1, prepend?: true)

  ## Options

  - `prepend?` - when `true`, places the hook before all other hooks instead of after

  ## See also

  - `after_action/2` for hooks that run after the action completes
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `Ash.read/2` for executing queries with hooks
  """
  @spec before_action(
          query :: t(),
          fun :: (t() -> t() | {t(), list(Ash.Notifier.Notification.t())}),
          opts :: Keyword.t()
        ) ::
          t()
  def before_action(query, func, opts \\ []) do
    query = new(query)

    if opts[:prepend?] do
      %{query | before_action: [func | query.before_action]}
    else
      %{query | before_action: query.before_action ++ [func]}
    end
  end

  @doc false
  @spec authorize_results(
          t(),
          (t(), [Ash.Resource.record()] ->
             {:ok, [Ash.Resource.record()]}
             | {:ok, [Ash.Resource.record()], list(Ash.Notifier.Notification.t())}
             | {:error, term})
        ) :: t()
  def authorize_results(query, func) do
    query = new(query)
    %{query | authorize_results: [func | query.authorize_results]}
  end

  # remove when 4.0 happens and `@read_action_after_action_hooks_in_order?` goes away
  @dialyzer {:nowarn_function, after_action: 2}

  @doc """
  Adds an after_action hook to the query.

  After action hooks are called with the query and the list of records returned
  from the action. They can modify the records, perform side effects, or return
  errors to halt processing. The hook can return notifications alongside the records.

  ## Examples

      # Transform records after loading
      iex> query = MyApp.Post
      ...> |> Ash.Query.after_action(fn query, records ->
      ...>   enriched_records = Enum.map(records, &add_computed_field/1)
      ...>   {:ok, enriched_records}
      ...> end)

      # Log successful reads
      iex> query = MyApp.Post
      ...> |> Ash.Query.after_action(fn query, records ->
      ...>   IO.puts("Successfully loaded \#{length(records)} posts")
      ...>   {:ok, records}
      ...> end)

      # Add notifications after the action
      iex> query = MyApp.Post
      ...> |> Ash.Query.after_action(fn query, records ->
      ...>   notifications = create_read_notifications(records)
      ...>   {:ok, records, notifications}
      ...> end)

      # Validate results and potentially error
      iex> query = MyApp.Post
      ...> |> Ash.Query.after_action(fn query, records ->
      ...>   if Enum.any?(records, &restricted?/1) do
      ...>     {:error, "Access denied to restricted posts"}
      ...>   else
      ...>     {:ok, records}
      ...>   end
      ...> end)

  ## See also

  - `before_action/3` for hooks that run before the action executes
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `Ash.read/2` for executing queries with hooks
  """
  @spec after_action(
          query :: t(),
          fun :: (t(), [Ash.Resource.record()] ->
                    {:ok, [Ash.Resource.record()]}
                    | {:ok, [Ash.Resource.record()], list(Ash.Notifier.Notification.t())}
                    | {:error, term})
        ) :: t()
  # in 4.0, add an option to prepend hooks
  def after_action(query, func) do
    query = new(query)

    if @read_action_after_action_hooks_in_order? do
      %{query | after_action: query.after_action ++ [func]}
    else
      %{query | after_action: [func | query.after_action]}
    end
  end

  defp add_action_filters(query, %{filter: nil}, _actor), do: query

  defp add_action_filters(query, action, actor) do
    if Ash.Expr.template_references_actor?(action.filter) and is_nil(actor) do
      Ash.Query.add_error(query, ReadActionRequiresActor.exception([]))
    else
      built_filter =
        Ash.Expr.fill_template(
          action.filter,
          actor: actor,
          tenant: query.to_tenant,
          args: query.arguments,
          context: query.context
        )

      do_filter(query, built_filter)
    end
  end

  @doc """
  Ensure that only the specified *attributes* are present in the results.

  The first call to `select/2` will replace the default behavior of selecting
  all attributes. Subsequent calls to `select/2` will combine the provided
  fields unless the `replace?` option is provided with a value of `true`.

  If a field has been deselected, selecting it again will override that (because a single list of fields is tracked for selection)

  Primary key attributes are always selected and cannot be deselected.

  When attempting to load a relationship (or manage it with `Ash.Changeset.manage_relationship/3`),
  if the source field is not selected on the query/provided data an error will be produced. If loading
  a relationship with a query, an error is produced if the query does not select the destination field
  of the relationship.

  Use `ensure_selected/2` if you wish to make sure a field has been selected, without deselecting any other fields.

  ## Examples

      # Select specific attributes
      iex> MyApp.Post |> Ash.Query.select([:title, :content])
      %Ash.Query{select: [:id, :title, :content], ...}

      # Select additional attributes (combines with existing selection)
      iex> MyApp.Post
      ...> |> Ash.Query.select([:title])
      ...> |> Ash.Query.select([:content])
      %Ash.Query{select: [:id, :title, :content], ...}

      # Replace existing selection
      iex> MyApp.Post
      ...> |> Ash.Query.select([:title])
      ...> |> Ash.Query.select([:content], replace?: true)
      %Ash.Query{select: [:id, :content], ...}

  ## See also

  - `ensure_selected/2` for adding fields without deselecting others
  - `deselect/2` for removing specific fields from selection
  - `load/3` for loading relationships and calculations
  """
  @spec select(t() | Ash.Resource.t(), list(atom) | atom, Keyword.t()) :: t()
  def select(query, fields, opts \\ []) do
    fields =
      case fields do
        %MapSet{} = fields -> fields
        fields -> MapSet.new(List.wrap(fields))
      end

    query = new(query)
    existing_fields = Ash.Resource.Info.attribute_names(query.resource)

    valid_fields = MapSet.intersection(fields, existing_fields)

    query =
      if MapSet.size(valid_fields) != MapSet.size(fields) do
        MapSet.difference(fields, existing_fields)
        |> Enum.reduce(query, fn field, query ->
          Ash.Query.add_error(
            query,
            Ash.Error.Query.NoSuchAttribute.exception(resource: query.resource, attribute: field)
          )
        end)
      else
        query
      end

    select =
      valid_fields
      |> MapSet.union(Ash.Resource.Info.always_selected_attribute_names(query.resource))
      |> MapSet.union(MapSet.new(Ash.Resource.Info.primary_key(query.resource)))

    new_select =
      if opts[:replace?] do
        select
      else
        MapSet.union(MapSet.new(query.select || []), select)
      end

    %{query | select: MapSet.to_list(new_select)}
  end

  @doc """
  Determines if the filter statement of a query is equivalent to the provided expression.

  This uses the satisfiability solver that is used when solving for policy authorizations. In complex scenarios, or when using
  custom database expressions, (like fragments in ash_postgres), this function may return `:maybe`. Use `supserset_of?` to always return
  a boolean.
  """
  defmacro equivalent_to(query, expr) do
    quote do
      require Ash.Expr
      query = unquote(query)
      expr = unquote(Ash.Expr.do_expr(expr))
      require Ash.Query

      case Ash.Query.superset_of(query, expr) do
        :maybe ->
          :maybe

        true ->
          Ash.Query.subset_of(query, expr)

        false ->
          false
      end
    end
  end

  @doc """
  Same as `equivalent_to/2` but always returns a boolean. `:maybe` returns `false`.
  """
  defmacro equivalent_to?(query, expr) do
    quote do
      Ash.Query.equivalent_to(unquote(query), unquote(expr)) == true
    end
  end

  @doc """
  Determines if the provided expression would return data that is a subset of the data returned by the filter on the query.

  This uses the satisfiability solver that is used when solving for policy authorizations. In complex scenarios, or when using
  custom database expressions, (like fragments in ash_postgres), this function may return `:maybe`. Use `supserset_of?` to always return
  a boolean.
  """
  defmacro superset_of(query, expr) do
    quote do
      query = unquote(query)
      require Ash.Expr
      expr = unquote(Ash.Expr.do_expr(expr))
      left_filter = query.filter

      {:ok, left_expression} =
        Ash.Filter.hydrate_refs(left_filter.expression, %{
          resource: query.resource,
          aggregates: query.aggregates,
          calculations: query.calculations,
          public?: false
        })

      left_filter = %{left_filter | expression: left_expression}

      {:ok, right_expression} =
        Ash.Filter.hydrate_refs(expr, %{
          resource: query.resource,
          aggregates: query.aggregates,
          calculations: query.calculations,
          public?: false
        })

      right_filter = %{left_filter | expression: right_expression}

      Ash.Filter.strict_subset(left_filter, right_filter)
    end
  end

  @doc """
  Same as `superset_of/2` but always returns a boolean. `:maybe` returns `false`.
  """
  defmacro superset_of?(query, expr) do
    quote do
      Ash.Query.superset_of(unquote(query), unquote(expr)) == true
    end
  end

  @doc """
  Determines if the provided expression would return data that is a subset of the data returned by the filter on the query.

  This uses the satisfiability solver that is used when solving for policy authorizations. In complex scenarios, or when using
  custom database expressions, (like fragments in ash_postgres), this function may return `:maybe`. Use `subset_of?` to always return
  a boolean.
  """
  defmacro subset_of(query, expr) do
    quote do
      query = unquote(query)
      require Ash.Expr
      expr = unquote(Ash.Expr.do_expr(expr))
      right_filter = query.filter

      {:ok, right_expression} =
        Ash.Filter.hydrate_refs(right_filter.expression, %{
          resource: query.resource,
          aggregates: query.aggregates,
          calculations: query.calculations,
          public?: false
        })

      right_filter = %{right_filter | expression: right_expression}

      {:ok, left_expression} =
        Ash.Filter.hydrate_refs(expr, %{
          resource: query.resource,
          aggregates: query.aggregates,
          calculations: query.calculations,
          public?: false
        })

      left_filter = %{right_filter | expression: left_expression}
      Ash.Filter.strict_subset(left_filter, right_filter)
    end
  end

  @doc """
  Same as `subset_of/2` but always returns a boolean. `:maybe` returns `false`.
  """
  defmacro subset_of?(query, expr) do
    quote do
      Ash.Query.subset_of(unquote(query), unquote(expr)) == true
    end
  end

  @doc """
  Ensures that the given attributes are selected.

  The first call to `select/2` will *limit* the fields to only the provided fields.
  Use `ensure_selected/2` to say "select this field (or these fields) without deselecting anything else".
  This function is additive - it will not remove any fields that are already selected.

  ## Examples

      # Ensure specific fields are selected (additive)
      iex> MyApp.Post |> Ash.Query.ensure_selected([:title])
      %Ash.Query{select: [:id, :title, :content, :created_at], ...}

      # Add to existing selection
      iex> MyApp.Post
      ...> |> Ash.Query.select([:title])
      ...> |> Ash.Query.ensure_selected([:content, :author_id])
      %Ash.Query{select: [:id, :title, :content, :author_id], ...}

      # Ensure fields for relationship loading
      iex> MyApp.Post
      ...> |> Ash.Query.ensure_selected([:author_id])
      ...> |> Ash.Query.load(:author)
      %Ash.Query{select: [..., :author_id], load: [author: []], ...}

  ## See also

  - `select/3` for explicitly controlling field selection
  - `deselect/2` for removing specific fields from selection
  - `load/3` for loading relationships that may require specific fields
  """
  @spec ensure_selected(t() | Ash.Resource.t(), list(atom) | atom) :: t()
  def ensure_selected(query, fields) do
    query = new(query)

    if query.select do
      Ash.Query.select(query, List.wrap(fields))
    else
      default_attributes = Ash.Resource.Info.selected_by_default_attribute_names(query.resource)

      query
      |> Ash.Query.select(default_attributes)
      |> Ash.Query.select(List.wrap(fields))
    end
  end

  @doc """
  Returns a list of attributes, aggregates, relationships, and calculations that are being loaded

  Provide a list of field types to narrow down the returned results.
  """
  def accessing(
        query,
        types \\ [:attributes, :relationships, :calculations, :aggregates],
        only_public? \\ true
      ) do
    query.resource
    |> Ash.Resource.Info.fields(types)
    |> then(fn fields ->
      if only_public? do
        Stream.filter(fields, & &1.public?)
      else
        fields
      end
    end)
    |> Stream.map(& &1.name)
    |> Enum.filter(&loading?(query, &1))
  end

  @doc """
  Ensures that the specified attributes are `nil` in the query results.

  This function removes specified fields from the selection, causing them to be
  excluded from the query results. If no fields are currently selected (meaning
  all fields would be returned by default), this will first select all default
  fields and then remove the specified ones.

  ## Examples

      # Remove specific fields from results
      iex> MyApp.Post |> Ash.Query.deselect([:content])
      %Ash.Query{select: [:id, :title, :created_at, ...], ...}

      # Remove multiple fields
      iex> MyApp.Post |> Ash.Query.deselect([:content, :metadata])
      %Ash.Query{select: [:id, :title, :created_at, ...], ...}

      # Deselect from existing selection
      iex> MyApp.Post
      ...> |> Ash.Query.select([:title, :content, :author_id])
      ...> |> Ash.Query.deselect([:content])
      %Ash.Query{select: [:id, :title, :author_id], ...}

      # Deselect empty list (no-op)
      iex> MyApp.Post |> Ash.Query.deselect([])
      %Ash.Query{...}

  ## See also

  - `select/3` for explicitly controlling field selection
  - `ensure_selected/2` for adding fields without removing others
  - Primary key fields cannot be deselected and will always be included
  """
  @spec deselect(t() | Ash.Resource.t(), list(atom)) :: t()
  def deselect(query, []), do: new(query)

  def deselect(query, fields) do
    query = new(query)

    select =
      if query.select do
        query.select -- List.wrap(fields)
      else
        MapSet.difference(
          Ash.Resource.Info.selected_by_default_attribute_names(query.resource),
          MapSet.new(List.wrap(fields))
        )
      end

    select(query, select, replace?: true)
  end

  @doc """
  Checks if a specific field is currently selected in the query.

  Returns `true` if the field will be included in the query results, either
  because it's explicitly selected, it's selected by default, or it's a
  primary key field (which are always selected).

  ## Examples

      # Check selection when no explicit select is set (uses defaults)
      iex> query = MyApp.Post |> Ash.Query.new()
      iex> Ash.Query.selecting?(query, :title)
      true

      # Check selection with explicit select
      iex> query = MyApp.Post |> Ash.Query.select([:title, :content])
      iex> Ash.Query.selecting?(query, :title)
      true
      iex> Ash.Query.selecting?(query, :metadata)
      false

      # Primary key fields are always selected
      iex> query = MyApp.Post |> Ash.Query.select([:title])
      iex> Ash.Query.selecting?(query, :id)  # assuming :id is primary key
      true

  ## See also

  - `select/3` for controlling field selection
  - `ensure_selected/2` for adding fields to selection
  - `load/3` for loading relationships that may require specific fields
  """
  @spec selecting?(t(), atom()) :: boolean()
  def selecting?(query, field) do
    case query.select do
      nil ->
        query.resource
        |> Ash.Resource.Info.attribute(field)
        |> case do
          %{select_by_default?: true} -> true
          _ -> false
        end

      select ->
        if field in select do
          true
        else
          attribute = Ash.Resource.Info.attribute(query.resource, field)

          attribute && attribute.primary_key?
        end
    end || loading?(query, field)
  end

  @doc """
  Returns true if the field/relationship or path to field/relationship is being loaded.

  It accepts an atom or a list of atoms, which is treated for as a "path", i.e:

      Resource |> Ash.Query.load(friends: [enemies: [:score]]) |> Ash.Query.loading?([:friends, :enemies, :score])
      iex> true

      Resource |> Ash.Query.load(friends: [enemies: [:score]]) |> Ash.Query.loading?([:friends, :score])
      iex> false

      Resource |> Ash.Query.load(friends: [enemies: [:score]]) |> Ash.Query.loading?(:friends)
      iex> true
  """
  def loading?(query, [last]) do
    loading?(query, last)
  end

  def loading?(query, [first | rest]) do
    case Keyword.get(query.load || [], first) do
      %Ash.Query{} = next -> loading?(next, rest)
      nil -> false
      other -> raise "Cannot check if loading path #{inspect(rest)} of #{inspect(other)}"
    end || loading_through?(query, [first | rest])
  end

  def loading?(query, item) when is_atom(item) do
    selecting? =
      if Ash.Resource.Info.attribute(query.resource, item) do
        is_nil(query.select) || item in query.select
      end

    selecting? || has_key?(query.load || [], item) ||
      Enum.any?(query.calculations, fn
        {_, %{module: Ash.Resource.Calculation.LoadRelationship, opts: opts}} ->
          opts[:relationship] == item

        {_, %{module: Ash.Resource.Calculation.LoadAttribute, opts: opts}} ->
          opts[:attribute] == item

        {_, %{calc_name: calc_name}} ->
          calc_name == item
      end) ||
      Enum.any?(query.aggregates, fn {_, %{agg_name: agg_name}} ->
        agg_name == item
      end)
  end

  defp loading_through?(query, [first | rest]) do
    Enum.any?(query.calculations, fn
      {_, %{module: Ash.Resource.Calculation.LoadRelationship, opts: opts}} ->
        if opts[:relationship] == first do
          if opts[:query] do
            Ash.Query.loading?(opts[:query], rest)
          end
        end

      {_, %{module: Ash.Resource.Calculation.LoadAttribute, opts: opts}} ->
        opts[:attribute] == first && opts[:load] && loading_via_keyword?(opts[:load], rest)
    end)
  end

  defp loading_via_keyword?(keyword, item) when is_atom(item) do
    Keyword.keyword?(keyword) and Keyword.has_key?(keyword, item)
  end

  defp loading_via_keyword?(keyword, [item]) do
    loading_via_keyword?(keyword, item)
  end

  defp loading_via_keyword?(keyword, [first | rest]) do
    Keyword.keyword?(keyword) and
      Enum.any?(keyword, fn {key, next} ->
        key == first && loading_via_keyword?(next, rest)
      end)
  end

  @doc """
  Adds a load statement to the result of an attribute or calculation.

  Uses `Ash.Type.load/5` to request that the type load nested data.
  """
  def load_through(query, type, name, load) when type in [:attribute, :calculation] do
    {attr_type, constraints} =
      if type == :calculation do
        calc = Map.get(query.calculations, name)
        {calc.type, calc.constraints}
      else
        attr =
          Ash.Resource.Info.attribute(query.resource, name)

        {attr.type, attr.constraints}
      end

    case Ash.Type.merge_load(
           attr_type,
           query.load_through[type][name] || [],
           load,
           constraints,
           nil
         ) do
      {:ok, new_value} ->
        Map.update!(query, :load_through, fn types ->
          types
          |> Map.put_new(type, %{})
          |> Map.update!(type, fn load_through ->
            Map.put(load_through, name, new_value)
          end)
        end)

      {:error, error} ->
        Ash.Query.add_error(query, error)

      :error ->
        Ash.Query.add_error(query, "Type #{type} has no exported function merge_load/4")
    end
  end

  @doc """
  Merges two query's load statements, for the purpose of handling calculation requirements.

  This should only be used if you are writing a custom type that is loadable.
  See the callback documentation for `c:Ash.Type.merge_load/4` for more.
  """
  def merge_query_load(left, right, context) do
    if context do
      Ash.Actions.Read.Calculations.merge_query_load(
        left,
        right,
        context.domain,
        context[:calc_path],
        context[:calc_name],
        context[:calc_load],
        context[:relationship_path],
        :error,
        context[:strict_loads?],
        context[:reuse_values?],
        context[:authorize?]
      )
    else
      load(left, right)
    end
  end

  @doc """
  Loads relationships, calculations, or aggregates on the resource.

  By default, loading attributes has no effect, as all attributes are returned.
  See the section below on "Strict Loading" for more.

  ## Examples

      # Load simple relationships
      iex> Ash.Query.load(MyApp.Post, :author)
      %Ash.Query{load: [author: []], ...}

      # Load nested relationships
      iex> Ash.Query.load(MyApp.Post, [comments: [:author, :ratings]])
      %Ash.Query{load: [comments: [author: [], ratings: []]], ...}

      # Load relationships with custom queries
      iex> author_query = Ash.Query.filter(MyApp.User, active: true)
      iex> Ash.Query.load(MyApp.Post, [comments: [author: author_query]])
      %Ash.Query{load: [comments: [author: %Ash.Query{...}]], ...}

      # Load calculations with arguments
      iex> Ash.Query.load(MyApp.User, full_name: %{format: :last_first})
      %Ash.Query{calculations: %{full_name: %Ash.Query.Calculation{...}}, ...}

  ## Strict Loading

  By passing `strict?: true`, only specified attributes will be loaded when passing
  a list of fields to fetch on a relationship, which allows for more optimized data-fetching.

      # Only load specific fields on relationships
      iex> Ash.Query.load(MyApp.Category, [:name, posts: [:title, :published_at]], strict?: true)
      %Ash.Query{load: [posts: [:title, :published_at]], ...}

  When using `strict?: true` and loading nested relationships, you must specify all the
  attributes you want to load alongside the nested relationships:

      # Must include needed attributes when loading nested relationships strictly
      iex> Ash.Query.load(MyApp.Post, [:title, :published_at, category: [:name]], strict?: true)
      %Ash.Query{...}

  ## See also

  - `select/3` for controlling which attributes are returned
  - `ensure_selected/2` for ensuring specific fields are selected
  - `Ash.read/2` for executing queries with loaded data
  - [Relationships Guide](/documentation/topics/resources/relationships.md) for understanding relationships
  - [Calculations Guide](/documentation/topics/resources/calculations.md) for understanding calculations
  """
  @spec load(
          t() | Ash.Resource.t(),
          atom
          | Ash.Query.Calculation.t()
          | Ash.Query.Aggregate.t()
          | list(atom | Ash.Query.Calculation.t() | Ash.Query.Aggregate.t())
          | list({atom | Ash.Query.Calculation.t() | Ash.Query.Aggregate.t(), term}),
          Keyword.t()
        ) ::
          t()

  def load(query, load_statement, opts \\ [])

  def load(query, %Ash.Query{} = new, opts) do
    query |> new() |> merge_load(new, opts)
  end

  def load(query, fields, opts) when not is_list(fields) do
    load(query, List.wrap(fields), opts)
  end

  def load(query, fields, opts) do
    query = new(query)

    Enum.reduce(fields, query, fn
      %Ash.Query{} = new, query ->
        merge_load(new, query, opts)

      [], query ->
        query

      {field, %__MODULE__{} = nested}, query ->
        if rel = Ash.Resource.Info.relationship(query.resource, field) do
          load_relationship(query, rel, nested, opts)
        else
          add_error(
            query,
            :load,
            Ash.Error.Query.InvalidLoad.exception(load: [{field, nested}])
          )
        end

      {field, {args, load_through}}, query ->
        if resource_calculation = Ash.Resource.Info.calculation(query.resource, field) do
          load_resource_calculation(query, resource_calculation, args, load_through)
        else
          add_error(
            query,
            :load,
            Ash.Error.Query.InvalidLoad.exception(load: [{field, {args, load_through}}])
          )
        end

      {field, rest}, query ->
        cond do
          rel = Ash.Resource.Info.relationship(query.resource, field) ->
            load_relationship(query, rel, rest, opts)

          resource_calculation = Ash.Resource.Info.calculation(query.resource, field) ->
            load_resource_calculation(query, resource_calculation, rest)

          attribute = Ash.Resource.Info.attribute(query.resource, field) ->
            if Ash.Type.can_load?(attribute.type, attribute.constraints) do
              query
              |> Ash.Query.ensure_selected(attribute.name)
              |> Ash.Query.load_through(:attribute, attribute.name, rest)
            else
              add_error(
                query,
                :load,
                Ash.Error.Query.InvalidLoad.exception(load: [{field, rest}])
              )
            end

          true ->
            add_error(query, :load, Ash.Error.Query.InvalidLoad.exception(load: field))
        end

      field, query ->
        do_load(query, field, opts)
    end)
  end

  defp load_resource_calculation(query, resource_calculation, args, load_through \\ nil) do
    if Keyword.keyword?(args) || is_map(args) do
      {name, load} =
        cond do
          Keyword.keyword?(args) ->
            case Keyword.fetch(args, :as) do
              {:ok, value} ->
                {value, nil}

              :error ->
                {resource_calculation.name, resource_calculation.name}
            end

          is_map(args) ->
            case Map.fetch(args, :as) do
              {:ok, value} ->
                {value, nil}

              :error ->
                {resource_calculation.name, resource_calculation.name}
            end

          true ->
            {resource_calculation.name, resource_calculation.name}
        end

      case Calculation.from_resource_calculation(query.resource, resource_calculation,
             args: Map.new(args),
             source_context: query.context
           ) do
        {:ok, calculation} ->
          calculation =
            select_and_load_calc(
              resource_calculation,
              %{calculation | load: load, name: name, calc_name: resource_calculation.name},
              query
            )

          query = Map.update!(query, :calculations, &Map.put(&1, name, calculation))

          if load_through do
            load_through(query, :calculation, name, load_through)
          else
            query
          end

        {:error, error} ->
          add_error(query, :load, error)
      end
    else
      add_error(
        query,
        Ash.Error.Query.InvalidLoad.exception(load: {resource_calculation.name, args})
      )
    end
  end

  @doc false
  def select_and_load_calc(resource_calculation, calculation, query) do
    module = calculation.module
    opts = calculation.opts

    if resource_calculation do
      resource_calculation_load =
        List.wrap(resource_calculation.load)

      loads =
        module.load(
          query,
          opts,
          Map.put(calculation.context, :context, query.context)
        )
        |> Ash.Actions.Helpers.validate_calculation_load!(module)
        |> Enum.concat(resource_calculation_load)

      %{calculation | required_loads: loads}
    else
      loads =
        module.load(
          query,
          opts,
          Map.put(calculation.context, :context, query.context)
        )
        |> Ash.Actions.Helpers.validate_calculation_load!(module)

      %{
        calculation
        | required_loads: Enum.concat(List.wrap(loads), List.wrap(calculation.required_loads))
      }
    end
  end

  defp fetch_key(map, key) when is_map(map) do
    Map.fetch(map, key)
  end

  defp fetch_key(keyword, key) do
    if Keyword.keyword?(keyword) do
      Keyword.fetch(keyword, key)
    else
      :error
    end
  end

  @doc false
  def resource_calc_to_calc(query, name, resource_calculation, args \\ %{}) do
    {name, load} =
      case fetch_key(args, :as) do
        :error -> {name, name}
        {:ok, key} -> {key, nil}
      end

    with {:ok, calculation} <-
           Ash.Query.Calculation.from_resource_calculation(query.resource, resource_calculation,
             source_context: query.context,
             args: Map.new(args)
           ) do
      {:ok,
       select_and_load_calc(
         resource_calculation,
         %{calculation | load: load, name: name},
         query
       )}
    end
  end

  defp do_load(query, field, opts) when is_list(field) do
    Enum.reduce(field, query, &do_load(&2, &1, opts))
  end

  defp do_load(query, field, opts) do
    cond do
      match?(%Ash.Query.Calculation{}, field) ->
        Map.update!(
          query,
          :calculations,
          &Map.put(
            &1,
            field.name,
            select_and_load_calc(nil, field, query)
          )
        )

      match?(%{__struct__: Ash.Query.Aggregate}, field) ->
        if Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, field.kind}) do
          Map.update!(
            query,
            :aggregates,
            &Map.put(
              &1,
              field.name,
              field
            )
          )
        else
          add_error(
            query,
            Ash.Error.Query.AggregatesNotSupported.exception(
              resource: query.resource,
              feature: "using"
            )
          )
        end

      is_atom(field) && Ash.Resource.Info.attribute(query.resource, field) ->
        ensure_selected(query, field)

      rel = Ash.Resource.Info.relationship(query.resource, field) ->
        load_relationship(query, rel, [], opts)

      aggregate = Ash.Resource.Info.aggregate(query.resource, field) ->
        can_do_aggregate? =
          if Map.get(aggregate, :related?, true) do
            Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, aggregate.kind})
          else
            Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, aggregate.kind}) &&
              Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, :unrelated})
          end

        with {:can?, true} <- {:can?, can_do_aggregate?},
             {:ok, query_aggregate} <-
               Aggregate.new(
                 query.resource,
                 aggregate.name,
                 aggregate.kind,
                 agg_name: aggregate.name,
                 path: aggregate.relationship_path,
                 query: [filter: aggregate.filter, sort: aggregate.sort],
                 field: aggregate.field,
                 default: aggregate.default,
                 filterable?: aggregate.filterable?,
                 type: aggregate.type,
                 constraints: aggregate.constraints,
                 implementation: aggregate.implementation,
                 include_nil?: aggregate.include_nil?,
                 uniq?: aggregate.uniq?,
                 read_action: aggregate.read_action,
                 authorize?: aggregate.authorize?,
                 sortable?: aggregate.sortable?,
                 sensitive?: aggregate.sensitive?,
                 join_filters:
                   Map.new(aggregate.join_filters, &{&1.relationship_path, &1.filter}),
                 resource: aggregate.resource,
                 related?: Map.get(aggregate, :related?, true)
               ) do
          query_aggregate = %{query_aggregate | load: field}
          new_aggregates = Map.put(query.aggregates, aggregate.name, query_aggregate)

          %{query | aggregates: new_aggregates}
        else
          {:error, error} ->
            add_error(
              query,
              :aggregates,
              Ash.Error.to_ash_error(error, nil,
                bread_crumbs: "Loading aggregate: #{inspect(field)} for query: #{inspect(query)}"
              )
            )

          {:can?, false} ->
            add_error(
              query,
              :aggregate,
              AggregatesNotSupported.exception(resource: query.resource, feature: "using")
            )
        end

      resource_calculation = Ash.Resource.Info.calculation(query.resource, field) ->
        load_resource_calculation(query, resource_calculation, %{})

      true ->
        add_error(query, :load, Ash.Error.Query.InvalidLoad.exception(load: field))
    end
  end

  @doc false
  def validate_calculation_arguments(calculation, args, allow_expr? \\ true) do
    args =
      if Keyword.keyword?(args) do
        Map.new(args)
      else
        args
      end

    args = Map.delete(args, :as)

    has_one_expr? = Enum.any?(args, fn {_, value} -> Ash.Expr.expr?(value) end)

    args
    |> Enum.reduce_while({:ok, %{}}, fn {key, value}, {:ok, arg_values} ->
      argument =
        if is_binary(key) do
          Enum.find(calculation.arguments, fn arg -> to_string(arg.name) == key end)
        else
          Enum.find(calculation.arguments, fn arg -> arg.name == key end)
        end

      value = Ash.Type.Helpers.handle_indexed_maps(argument.type, value)

      cond do
        !argument ->
          error_calc =
            case calculation do
              %{calc_name: calc_name} ->
                calc_name

              %Ash.Resource.Calculation{name: name} ->
                name

              calc ->
                calc
            end

          {:halt,
           {:error,
            Ash.Error.Invalid.NoSuchInput.exception(
              calculation: error_calc,
              input: key,
              inputs: Enum.map(calculation.arguments, & &1.name)
            )}}

        Ash.Expr.expr?(value) && argument.allow_expr? && allow_expr? ->
          {:cont,
           {:ok,
            Map.put(
              arg_values,
              argument.name,
              %Ash.Query.Function.Type{
                arguments: [value, argument.type, argument.constraints]
              }
            )}}

        Ash.Expr.expr?(value) ->
          {:halt,
           {:error,
            InvalidCalculationArgument.exception(
              field: argument.name,
              calculation: calculation.name,
              message: "does not support expressions",
              value: value
            )}}

        is_nil(value) && argument.allow_nil? ->
          {:cont, {:ok, Map.put(arg_values, argument.name, nil)}}

        is_nil(value) && is_nil(argument.default) ->
          {:halt,
           {:error,
            InvalidCalculationArgument.exception(
              field: argument.name,
              calculation: calculation.name,
              message: "is required",
              value: value
            )}}

        is_nil(Map.get(args, argument.name, Map.get(args, to_string(argument.name)))) &&
            not is_nil(value) ->
          if has_one_expr? do
            {:cont,
             {:ok,
              Map.put(
                arg_values,
                argument.name,
                %Ash.Query.Function.Type{
                  arguments: [value, argument.type, argument.constraints]
                }
              )}}
          else
            {:cont,
             {:ok,
              Map.put(
                arg_values,
                argument.name,
                value
              )}}
          end

        true ->
          with {:ok, casted} <- Ash.Type.cast_input(argument.type, value, argument.constraints),
               {:ok, casted} <-
                 Ash.Type.apply_constraints(argument.type, casted, argument.constraints) do
            if has_one_expr? do
              {:cont,
               {:ok,
                Map.put(
                  arg_values,
                  argument.name,
                  %Ash.Query.Function.Type{
                    arguments: [value, argument.type, argument.constraints]
                  }
                )}}
            else
              cond do
                is_nil(casted) && argument.allow_nil? ->
                  {:cont, {:ok, Map.put(arg_values, argument.name, nil)}}

                is_nil(casted) && is_nil(argument.default) ->
                  {:halt,
                   {:error,
                    InvalidCalculationArgument.exception(
                      field: argument.name,
                      calculation: calculation.name,
                      message: "is required",
                      value: value
                    )}}

                is_nil(Map.get(args, argument.name, Map.get(args, to_string(argument.name)))) &&
                    not is_nil(value) ->
                  {:cont,
                   {:ok,
                    Map.put(
                      arg_values,
                      argument.name,
                      value
                    )}}

                true ->
                  {:cont, {:ok, Map.put(arg_values, argument.name, casted)}}
              end
            end
          else
            {:error, error} when is_binary(error) ->
              {:halt,
               {:error,
                InvalidCalculationArgument.exception(
                  field: argument.name,
                  calculation: calculation.name,
                  message: error,
                  value: value
                )}}

            {:error, error} ->
              {:halt, {:error, Ash.Error.to_ash_error(error)}}
          end
      end
    end)
    |> set_defaults(calculation)
  end

  defp set_defaults({:ok, inputs}, calculation) do
    Enum.reduce_while(calculation.arguments, {:ok, inputs}, fn argument, {:ok, inputs} ->
      value =
        case Map.fetch(inputs, argument.name) do
          :error -> Map.fetch(inputs, to_string(argument.name))
          {:ok, value} -> {:ok, value}
        end

      case value do
        {:ok, value} ->
          if is_nil(value) && !argument.allow_nil? do
            {:halt,
             {:error,
              InvalidCalculationArgument.exception(
                field: argument.name,
                calculation: calculation.name,
                message: "is required",
                value: nil
              )}}
          else
            {:cont, {:ok, inputs}}
          end

        :error ->
          value = calc_arg_default(argument.default)

          if is_nil(value) && !argument.allow_nil? do
            {:halt,
             {:error,
              InvalidCalculationArgument.exception(
                field: argument.name,
                calculation: calculation.name,
                message: "is required",
                value: value
              )}}
          else
            {:cont, {:ok, Map.put(inputs, argument.name, value)}}
          end
      end
    end)
  end

  defp set_defaults(inputs, _), do: inputs

  defp calc_arg_default({module, function, args}), do: apply(module, function, args)
  defp calc_arg_default(value) when is_function(value, 0), do: value.()
  defp calc_arg_default(value), do: value

  @doc """
  Sets a specific context key to a specific value.

  Context is used to pass additional information through the query pipeline
  that can be accessed by preparations, calculations, and other query logic.
  This function adds or updates a single key in the query's context map.

  ## Examples

      # Add actor information to context
      iex> query = MyApp.Post |> Ash.Query.put_context(:actor, current_user)
      %Ash.Query{context: %{actor: %User{...}}, ...}

      # Add custom metadata for preparations
      iex> query = MyApp.Post |> Ash.Query.put_context(:source, "api")
      %Ash.Query{context: %{source: "api"}, ...}

      # Chain multiple context additions
      iex> MyApp.Post
      ...> |> Ash.Query.put_context(:tenant, "org_123")
      ...> |> Ash.Query.put_context(:locale, "en_US")
      %Ash.Query{context: %{tenant: "org_123", locale: "en_US"}, ...}

  ## See also

  - `set_context/2` for setting the entire context map
  - `for_read/4` for passing context when creating queries
  - Preparations and calculations can access context for custom logic
  """
  @spec put_context(t() | Ash.Resource.t(), atom, term) :: t()
  def put_context(query, key, value) do
    query = new(query)
    set_context(query, %{key => value})
  end

  @doc """
  Set the result of the action. This will prevent running the underlying datalayer behavior
  """
  @spec set_result(t(), term) :: t()
  def set_result(query, result) do
    set_context(query, %{private: %{action_result: result}})
  end

  @doc """
  Removes a result set previously with `set_result/2`
  """
  @spec clear_result(t()) :: t()
  def clear_result(query) do
    %{
      query
      | context: Map.update(query.context, :private, %{}, &Map.delete(&1, :action_result))
    }
  end

  @doc """
  Merge a map of values into the query context
  """
  @spec set_context(t() | Ash.Resource.t(), map | nil) :: t()
  def set_context(query, nil), do: new(query)

  def set_context(query, map) do
    query = new(query)

    %{
      query
      | context:
          query.context
          |> Ash.Helpers.deep_merge_maps(map)
          |> then(&Ash.Helpers.deep_merge_maps(&1, map[:shared] || %{}))
    }
  end

  @doc """
  Gets the value of an argument provided to the query.

  Returns the argument value if found, or `nil` if not found. Arguments can be
  provided when creating queries with `for_read/4` and are used by action logic
  such as preparations and filters.

  ## Examples

      # Get an argument that exists
      iex> query = Ash.Query.for_read(MyApp.Post, :published, %{since: ~D[2023-01-01]})
      iex> Ash.Query.get_argument(query, :since)
      ~D[2023-01-01]

      # Get an argument that doesn't exist
      iex> query = Ash.Query.for_read(MyApp.Post, :published, %{})
      iex> Ash.Query.get_argument(query, :since)
      nil

      # Arguments can be accessed by string or atom key
      iex> query = Ash.Query.for_read(MyApp.Post, :search, %{"query" => "elixir"})
      iex> Ash.Query.get_argument(query, :query)
      "elixir"

  ## See also

  - `fetch_argument/2` for safer argument access with explicit error handling
  - `set_argument/3` for adding arguments to queries
  - `for_read/4` for creating queries with arguments
  """
  @spec get_argument(t, atom | String.t()) :: term
  def get_argument(query, argument) when is_atom(argument) or is_binary(argument) do
    case fetch_argument(query, argument) do
      {:ok, value} -> value
      :error -> nil
    end
  end

  @doc """
  Fetches the value of an argument provided to the query.

  Returns `{:ok, value}` if the argument exists, or `:error` if not found.
  This is the safer alternative to `get_argument/2` when you need to distinguish
  between a `nil` value and a missing argument.

  ## Examples

      # Fetch an argument that exists
      iex> query = Ash.Query.for_read(MyApp.Post, :published, %{since: ~D[2023-01-01]})
      iex> Ash.Query.fetch_argument(query, :since)
      {:ok, ~D[2023-01-01]}

      # Fetch an argument that doesn't exist
      iex> query = Ash.Query.for_read(MyApp.Post, :published, %{})
      iex> Ash.Query.fetch_argument(query, :since)
      :error

      # Distinguish between nil and missing arguments
      iex> query = Ash.Query.for_read(MyApp.Post, :search, %{query: nil})
      iex> Ash.Query.fetch_argument(query, :query)
      {:ok, nil}

  ## See also

  - `get_argument/2` for simpler argument access
  - `set_argument/3` for adding arguments to queries
  - `for_read/4` for creating queries with arguments
  """
  @spec fetch_argument(t, atom | String.t()) :: {:ok, term} | :error
  def fetch_argument(query, argument) when is_atom(argument) or is_binary(argument) do
    query = new(query)

    case Map.fetch(query.arguments, argument) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case Map.fetch(query.arguments, to_string(argument)) do
          {:ok, value} -> {:ok, value}
          :error -> :error
        end
    end
  end

  @doc """
  Adds an argument to the query.

  Arguments are used by action logic such as preparations, filters, and other
  query modifications. They become available in filter templates and can be
  referenced in action configurations. Setting an argument after a query has
  been validated for an action will result in an error.

  ## Examples

      # Set an argument for use in action filters
      iex> query = Ash.Query.new(MyApp.Post)
      iex> Ash.Query.set_argument(query, :author_id, 123)
      %Ash.Query{arguments: %{author_id: 123}, ...}

      # Set multiple arguments by chaining
      iex> MyApp.Post
      ...> |> Ash.Query.set_argument(:category, "tech")
      ...> |> Ash.Query.set_argument(:published, true)
      %Ash.Query{arguments: %{category: "tech", published: true}, ...}

      # Arguments are used in action preparations and filters
      iex> query = MyApp.Post
      ...> |> Ash.Query.for_read(:by_author, %{author_id: 123})
      ...> |> Ash.Query.set_argument(:include_drafts, false)
      %Ash.Query{arguments: %{author_id: 123, include_drafts: false}, ...}

  ## See also

  - `get_argument/2` for retrieving argument values
  - `fetch_argument/2` for safe argument retrieval
  - `for_read/4` for creating queries with initial arguments
  """
  def set_argument(query, argument, value) do
    maybe_already_validated_error!(query)
    query = new(query)

    if query.action do
      argument =
        Enum.find(
          query.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      with {:arg, argument} when not is_nil(argument) <- {:arg, argument},
           value <- Ash.Type.Helpers.handle_indexed_maps(argument.type, value),
           constraints <- Ash.Type.include_source(argument.type, query, argument.constraints),
           {:ok, casted} <-
             Ash.Type.cast_input(argument.type, value, constraints),
           {:constrained, {:ok, casted}, argument} when not is_nil(casted) <-
             {:constrained, Ash.Type.apply_constraints(argument.type, casted, constraints),
              argument} do
        %{query | arguments: Map.put(query.arguments, argument.name, casted)}
      else
        {:arg, nil} ->
          query

        {:constrained, {:ok, nil}, argument} ->
          %{query | arguments: Map.put(query.arguments, argument.name, nil)}

        {:constrained, {:error, error}, argument} ->
          query = %{query | arguments: Map.put(query.arguments, argument.name, value)}
          add_invalid_errors(value, query, argument, error)

        {:error, error} ->
          query = %{query | arguments: Map.put(query.arguments, argument.name, value)}
          add_invalid_errors(value, query, argument, error)

        :error ->
          query = %{query | arguments: Map.put(query.arguments, argument.name, value)}
          add_invalid_errors(value, query, argument, "is invalid")
      end
    else
      %{query | arguments: Map.put(query.arguments, argument, value)}
    end
  end

  defp reset_arguments(%{arguments: arguments} = query) do
    Enum.reduce(arguments, query, fn {key, value}, query ->
      set_argument(query, key, value)
    end)
  end

  defp add_invalid_errors(value, query, argument, error) do
    query = %{query | invalid_keys: MapSet.put(query.invalid_keys, argument.name)}

    messages =
      if Keyword.keyword?(error) do
        [error]
      else
        List.wrap(error)
      end

    messages
    |> Enum.reduce(query, fn message, query ->
      message
      |> Ash.Type.Helpers.error_to_exception_opts(argument)
      |> Enum.reduce(query, fn opts, query ->
        add_error(
          query,
          InvalidArgument.exception(
            value: value,
            field: Keyword.get(opts, :field),
            message: Keyword.get(opts, :message),
            vars: opts
          )
        )
      end)
    end)
  end

  @doc """
  Remove an argument from the query
  """
  def delete_argument(query, argument_or_arguments) do
    query = new(query)

    argument_or_arguments
    |> List.wrap()
    |> Enum.reduce(query, fn argument, query ->
      %{query | arguments: Map.delete(query.arguments, argument)}
    end)
  end

  @doc """
  Merge a map of arguments to the arguments list
  """
  def set_arguments(query, map) do
    query = new(query)
    %{query | arguments: Map.merge(query.arguments, map)}
  end

  defp argument_default(value) when is_function(value, 0), do: value.()
  defp argument_default(value), do: value

  @doc """
  Sets the tenant for the query.

  In multitenant applications, this function configures which tenant's data
  the query should operate on. The tenant value is used to filter data
  and ensure proper data isolation between tenants.

  ## Examples

      # Set tenant using a string identifier
      iex> MyApp.Post |> Ash.Query.set_tenant("org_123")
      %Ash.Query{tenant: "org_123", ...}

      # Set tenant using a struct that implements Ash.ToTenant
      iex> org = %MyApp.Organization{id: 456}
      iex> MyApp.Post |> Ash.Query.set_tenant(org)
      %Ash.Query{tenant: %MyApp.Organization{id: 456}, ...}

      # Use with other query functions
      iex> MyApp.Post
      ...> |> Ash.Query.set_tenant("org_123")
      ...> |> Ash.Query.filter(published: true)
      %Ash.Query{tenant: "org_123", ...}

  ## See also

  - `for_read/4` for setting tenant when creating queries
  - `Ash.ToTenant` protocol for custom tenant conversion
  - `put_context/3` for adding tenant to query context
  """
  @spec set_tenant(t() | Ash.Resource.t(), Ash.ToTenant.t()) :: t()
  def set_tenant(query, tenant) do
    query = new(query)
    %{query | tenant: tenant, to_tenant: Ash.ToTenant.to_tenant(tenant, query.resource)}
  end

  @doc """
  Sets the pagination options of the query.

  This function configures how results should be paginated when the query is executed.
  Ash supports both offset-based pagination (limit/offset) and keyset-based pagination
  (cursor-based), with keyset being more efficient for large datasets.

  ## Examples

      # Offset-based pagination (page 2, 10 items per page)
      iex> MyApp.Post
      ...> |> Ash.Query.page(limit: 10, offset: 10)
      %Ash.Query{page: [limit: 10, offset: 10], ...}

      # Keyset pagination with before/after cursors
      iex> MyApp.Post
      ...> |> Ash.Query.sort(:created_at)
      ...> |> Ash.Query.page(limit: 20, after: "cursor_string")
      %Ash.Query{page: [limit: 20, after: "cursor_string"], ...}

      # Disable pagination (return all results)
      iex> MyApp.Post |> Ash.Query.page(nil)
      %Ash.Query{page: nil, ...}

      # Pagination with counting
      iex> MyApp.Post |> Ash.Query.page(limit: 10, count: true)
      %Ash.Query{page: [limit: 10, count: true], ...}

  ## Pagination Types

  ### Limit/offset pagination
  #{Spark.Options.docs(Ash.Page.Offset.page_opts())}

  ### Keyset pagination
  #{Spark.Options.docs(Ash.Page.Keyset.page_opts())}

  ## See also

  - `limit/2` and `offset/2` for simple pagination without page metadata
  - `sort/3` for ordering results (required for keyset pagination)
  - `Ash.read/2` for executing paginated queries
  """
  @spec page(t() | Ash.Resource.t(), Keyword.t() | nil | false) :: t()
  def page(query, page_opts) do
    query = new(query)

    case Ash.Page.page_opts(page_opts) do
      {:ok, page_opts} -> %{query | page: page_opts}
      {:error, _error} -> add_error(query, :page, InvalidPage.exception(page: page_opts))
    end
  end

  @doc "Removes a field from the list of fields to load"
  @spec unload(t(), list(atom)) :: t()
  def unload(query, fields) do
    query = new(query)

    Enum.reduce(fields, query, fn field, query ->
      case field do
        {field, rest} ->
          new_loads = do_unload_load(query.load, {field, rest})
          %{query | load: new_loads}

        field ->
          do_unload(query, field)
      end
    end)
  end

  defp do_unload(query, field) do
    cond do
      Ash.Resource.Info.attribute(query.resource, field) ->
        query

      Ash.Resource.Info.relationship(query.resource, field) ->
        %{query | load: Keyword.delete(query.load, field)}

      Ash.Resource.Info.aggregate(query.resource, field) ->
        new_aggregates =
          Enum.reduce(query.aggregates, %{}, fn
            {_field, %{load: ^field}}, acc ->
              acc

            {field, aggregate}, acc ->
              Map.put(acc, field, aggregate)
          end)

        %{query | aggregates: new_aggregates}

      Ash.Resource.Info.calculation(query.resource, field) ->
        new_calculations =
          Enum.reduce(query.calculations, %{}, fn
            {_field, %{load: ^field}}, acc ->
              acc

            {field, calculation}, acc ->
              Map.put(acc, field, calculation)
          end)

        %{query | calculations: new_calculations}

      true ->
        query
        |> Map.update!(:calculations, &Map.delete(&1, field))
        |> Map.update!(:aggregates, &Map.delete(&1, field))
    end
  end

  defp do_unload_load(%__MODULE__{} = query, unload) do
    %{query | load: do_unload_load(query.load, unload)}
  end

  defp do_unload_load(loads, {field, rest}) do
    Enum.reduce(loads, [], fn
      ^field, acc ->
        acc

      {^field, value}, acc ->
        new_value =
          rest
          |> List.wrap()
          |> Enum.reduce(value, &do_unload_load(&2, &1))

        [{field, new_value} | acc]

      value, acc ->
        [value | acc]
    end)
    |> Enum.reverse()
  end

  defp do_unload_load(loads, field) do
    do_unload_load(loads, {field, []})
  end

  @build_opts [
    filter: [
      type: :any,
      doc: "A filter keyword, map or expression"
    ],
    filter_input: [
      type: :any,
      doc: "A filter keyword or map, provided as input from an external source"
    ],
    sort: [
      type: :any,
      doc: "A sort list or keyword"
    ],
    sort_input: [
      type: :any,
      doc: "A sort list or keyword, provided as input from an external source"
    ],
    default_sort: [
      type: :any,
      doc:
        "A sort list or keyword to apply only if no other sort is specified, So if you apply any `sort`, this will be ignored."
    ],
    distinct_sort: [
      type: :any,
      doc: "A distinct_sort list or keyword"
    ],
    limit: [
      type: :integer,
      doc: "A limit to apply"
    ],
    offset: [
      type: :integer,
      doc: "An offset to apply"
    ],
    load: [
      type: :any,
      doc: "A load statement to add to the query"
    ],
    strict_load: [
      type: :any,
      doc: "A load statement to add to the query with the `strict?` option set to `true`"
    ],
    select: [
      type: :any,
      doc: "A select statement to add to the query"
    ],
    ensure_selected: [
      type: :any,
      doc: "An ensure_selected statement to add to the query"
    ],
    aggregate: [
      type: :any,
      doc:
        "A custom aggregate to add to the query. Can be `{name, type, relationship}` or `{name, type, relationship, build_opts}`"
    ],
    calculate: [
      type: :any,
      doc:
        "A custom calculation to add to the query. Can be `{name, module_and_opts}` or `{name, module_and_opts, context}`"
    ],
    distinct: [
      type: {:list, :atom},
      doc: "A distinct clause to add to the query"
    ],
    context: [
      type: :map,
      doc: "A map to merge into the query context"
    ]
  ]

  @doc false
  def build_opts, do: @build_opts

  @doc """
  Builds a query from a keyword list.

  This is used by certain query constructs like aggregates. It can also be used to manipulate a data structure
  before passing it to an ash query. It allows for building an entire query struct using only a keyword list.

  For example:

  ```elixir
  Ash.Query.build(MyResource, filter: [name: "fred"], sort: [name: :asc], load: [:foo, :bar], offset: 10)
  ```

  If you want to use the expression style filters, you can use `expr/1`.

  For example:

  ```elixir
  import Ash.Expr, only: [expr: 1]

  Ash.Query.build(Myresource, filter: expr(name == "marge"))
  ```

  ## Options

  #{Spark.Options.docs(@build_opts)}
  """
  @spec build(Ash.Resource.t() | t(), Ash.Domain.t() | nil, Keyword.t()) :: t()
  @spec build(Ash.Resource.t() | t(), Keyword.t()) :: t()
  def build(resource, domain \\ nil, keyword) do
    query =
      resource
      |> new()
      |> then(fn query ->
        if domain do
          set_domain(query, domain)
        else
          query
        end
      end)

    Enum.reduce(keyword, query, fn
      {:filter, value}, query ->
        do_filter(query, value)

      {:filter_input, value}, query ->
        filter_input(query, value)

      {:sort, value}, query ->
        sort(query, value)

      {:sort_input, value}, query ->
        sort_input(query, value)

      {:default_sort, value}, query ->
        default_sort(query, value)

      {:distinct_sort, value}, query ->
        distinct_sort(query, value)

      {:limit, value}, query ->
        limit(query, value)

      {:offset, value}, query ->
        offset(query, value)

      {:load, value}, query ->
        load(query, value)

      {:strict_load, value}, query ->
        load(query, value, strict?: true)

      {:distinct, value}, query ->
        distinct(query, value)

      {:lock, lock_type}, query ->
        lock(query, lock_type)

      {:aggregate, {name, type, relationship}}, query ->
        aggregate(query, name, type, relationship)

      {:aggregate, {name, type, relationship, agg_query}}, query ->
        aggregate(query, name, type, relationship, agg_query)

      {:calculate, {name, type, module_and_opts}}, query ->
        calculate(query, name, type, module_and_opts)

      {:calculate, {name, type, module_and_opts, arguments}}, query ->
        calculate(query, name, type, module_and_opts, arguments)

      {:select, fields}, query ->
        select(query, fields)

      {:deselect, fields}, query ->
        deselect(query, fields)

      {:ensure_selected, fields}, query ->
        ensure_selected(query, fields)

      {:context, context}, query ->
        set_context(query, context)
    end)
  end

  @doc "Set the query's domain, and any loaded query's domain"
  def set_domain(query, domain) do
    query = new(query)
    %{query | domain: domain, load: set_load_domain(query.load, domain)}
  end

  @doc """
  Adds an aggregation to the query.

  Aggregations are made available on the `aggregates` field of the records returned.
  They allow you to compute values from related data without loading entire relationships,
  making them very efficient for statistical operations.

  ## Examples

      # Count related records
      iex> Ash.Query.aggregate(MyApp.Author, :post_count, :count, :posts)
      %Ash.Query{aggregates: %{post_count: %Ash.Query.Aggregate{...}}, ...}

      # Sum values from related records
      iex> Ash.Query.aggregate(MyApp.Author, :total_likes, :sum, :posts, field: :like_count)
      %Ash.Query{aggregates: %{total_likes: %Ash.Query.Aggregate{...}}, ...}

      # Average with filtered aggregation
      iex> published_query = Ash.Query.filter(MyApp.Post, published: true)
      iex> Ash.Query.aggregate(MyApp.Author, :avg_published_likes, :avg, :posts,
      ...>   field: :like_count, query: published_query)
      %Ash.Query{aggregates: %{avg_published_likes: %Ash.Query.Aggregate{...}}, ...}

      # Count with default value
      iex> Ash.Query.aggregate(MyApp.Author, :post_count, :count, :posts, default: 0)
      %Ash.Query{aggregates: %{post_count: %Ash.Query.Aggregate{...}}, ...}

  ## Options

    * `query` - The query over the destination resource to use as a base for aggregation
    * `field` - The field to use for the aggregate. Not necessary for all aggregate types
    * `default` - The default value to use if the aggregate returns nil
    * `filterable?` - Whether or not this aggregate may be referenced in filters
    * `type` - The type of the aggregate
    * `constraints` - Type constraints for the aggregate's type
    * `implementation` - An implementation used when the aggregate kind is custom
    * `read_action` - The read action to use on the destination resource
    * `authorize?` - Whether or not to authorize access to this aggregate
    * `join_filters` - A map of relationship paths to filter expressions

  ## See also

  - [Resource DSL aggregates documentation](dsl-ash-resource.html#aggregates) for more information
  - `load/3` for loading relationships instead of aggregating
  - `calculate/8` for custom calculations
  - `Ash.read/2` for executing queries with aggregates
  """
  @spec aggregate(t() | Ash.Resource.t(), atom() | String.t(), atom(), atom()) :: t()
  def aggregate(query, name, kind, relationship) do
    aggregate(query, name, kind, relationship, [])
  end

  @spec aggregate(t() | Ash.Resource.t(), atom() | String.t(), atom(), atom(), Keyword.t()) :: t()

  def aggregate(query, name, kind, relationship, opts) when is_list(opts) do
    agg_query = opts[:query]
    default = opts[:default]
    filterable? = Keyword.get(opts, :filterable?, true)
    sortable? = Keyword.get(opts, :filterable?, true)
    type = opts[:type]
    constraints = opts[:constraints] || []
    implementation = opts[:implementation]
    include_nil? = Keyword.get(opts, :include_nil?, false)
    uniq? = opts[:uniq?]
    read_action = opts[:read_action]
    authorize? = Keyword.get(opts, :authorize?, true)
    join_filters = Keyword.get(opts, :join_filters, %{})
    sensitive? = Keyword.get(opts, :sensitive?, false)

    {field, agg_query} =
      case agg_query do
        %Ash.Query{} = query ->
          {opts[:field], query}

        agg_query ->
          Keyword.pop(agg_query || [], :field, opts[:field])
      end

    query = new(query)
    relationship = List.wrap(relationship)

    {related, actual_relationship, opts_with_resource, is_unrelated?} =
      case relationship do
        [module] when is_atom(module) ->
          if function_exported?(module, :__info__, 1) do
            {module, [], Keyword.put(opts, :resource, module), true}
          else
            related = Ash.Resource.Info.related(query.resource, relationship)
            {related, relationship, opts, false}
          end

        _ ->
          # Regular relationship path
          related = Ash.Resource.Info.related(query.resource, relationship)
          {related, relationship, opts, false}
      end

    # Check data layer capabilities
    can_do_aggregate? =
      if is_unrelated? do
        Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, kind}) &&
          Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, :unrelated})
      else
        # For related aggregates, just check the aggregate kind
        Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, kind})
      end

    if can_do_aggregate? do
      agg_query =
        case agg_query do
          [] ->
            read_action = Ash.Resource.Info.primary_action!(related, :read).name

            related
            |> for_read(read_action)

          options when is_list(options) ->
            read_action = Ash.Resource.Info.primary_action!(related, :read).name

            related
            |> for_read(read_action)
            |> Ash.Query.Aggregate.build_query(query.resource, options)

          %Ash.Query{} = query ->
            query
        end

      case Aggregate.new(
             query.resource,
             name,
             kind,
             Keyword.merge(
               [
                 path: actual_relationship,
                 query: agg_query,
                 field: field,
                 default: default,
                 filterable?: filterable?,
                 sortable?: sortable?,
                 sensitive?: sensitive?,
                 include_nil?: include_nil?,
                 type: type,
                 constraints: constraints,
                 implementation: implementation,
                 uniq?: uniq?,
                 read_action: read_action,
                 authorize?: authorize?,
                 join_filters: join_filters
               ],
               opts_with_resource
             )
           ) do
        {:ok, aggregate} ->
          new_aggregates = Map.put(query.aggregates, aggregate.name, aggregate)

          %{query | aggregates: new_aggregates}

        {:error, error} ->
          add_error(query, :aggregate, error)
      end
    else
      add_error(
        query,
        :aggregate,
        AggregatesNotSupported.exception(resource: query.resource, feature: "using #{kind}")
      )
    end
  end

  @doc """
  Adds a calculation to the query.

  Calculations are made available on the `calculations` field of the records returned.
  They allow you to compute dynamic values based on record data, other fields, or
  external information at query time.

  The `module_and_opts` argument accepts either a `module` or a `{module, opts}`. For more information
  on what that module should look like, see `Ash.Resource.Calculation`.

  ## Examples

      # Add a simple calculation
      iex> Ash.Query.calculate(MyApp.User, :display_name, :string,
      ...>   {MyApp.Calculations.DisplayName, []})
      %Ash.Query{calculations: %{display_name: %{...}}, ...}

      # Add calculation with arguments
      iex> Ash.Query.calculate(MyApp.Post, :word_count, :integer,
      ...>   {MyApp.Calculations.WordCount, []}, %{field: :content})
      %Ash.Query{calculations: %{word_count: %{...}}, ...}

      # Add calculation with constraints and context
      iex> Ash.Query.calculate(MyApp.Product, :discounted_price, :decimal,
      ...>   {MyApp.Calculations.Discount, []}, %{rate: 0.1},
      ...>   [precision: 2, scale: 2], %{currency: "USD"})
      %Ash.Query{calculations: %{discounted_price: %{...}}, ...}

  ## See also

  - `Ash.Resource.Calculation` for implementing custom calculations
  - `aggregate/5` for computing values from related records
  - `load/3` for loading predefined calculations from the resource
  - `select/3` for controlling which fields are returned alongside calculations
  """
  @spec calculate(
          t() | Ash.Resource.t(),
          atom(),
          Ash.Type.t(),
          module() | {module(), Keyword.t()},
          map(),
          Keyword.t(),
          map(),
          Keyword.t()
        ) :: t()
  def calculate(
        query,
        name,
        type,
        module_and_opts,
        arguments \\ %{},
        constraints \\ [],
        extra_context \\ %{},
        new_calculation_opts \\ []
      ) do
    query = new(query)

    {module, opts} =
      case module_and_opts do
        {module, opts} ->
          if Ash.Expr.expr?({module, opts}) do
            {Ash.Resource.Calculation.Expression, expr: {module, opts}}
          else
            {module, opts}
          end

        module when is_atom(module) ->
          {module, []}

        value ->
          {Ash.Resource.Calculation.Expression, expr: value}
      end

    case Calculation.new(
           name,
           module,
           opts,
           type,
           constraints,
           Keyword.merge(
             [arguments: arguments, source_context: query.context],
             new_calculation_opts
           )
         ) do
      {:ok, calculation} ->
        context = %{
          calculation.context
          | actor: Map.get(extra_context, :actor),
            tenant: Map.get(extra_context, :tenant),
            tracer: Map.get(extra_context, :tracer),
            authorize?: Map.get(extra_context, :authorize?)
        }

        calculation = %{calculation | context: context}

        loads =
          module.load(
            query,
            opts,
            calculation.context
          )
          |> Ash.Actions.Helpers.validate_calculation_load!(module)
          |> Enum.concat(List.wrap(calculation.required_loads))

        calculation = %{calculation | required_loads: loads}
        %{query | calculations: Map.put(query.calculations, name, calculation)}

      {:error, error} ->
        add_error(query, :calculations, error)
    end
  end

  @doc """
  Apply a sort only if no sort has been specified yet.

  This is useful for providing default sorts that can be overridden.

  ## Examples

  ```elixir
  # This will sort by name if no sort has been specified
  Ash.Query.default_sort(query, :name)

  # This will sort by name descending if no sort has been specified
  Ash.Query.default_sort(query, name: :desc)
  ```
  """
  @spec default_sort(t() | Ash.Resource.t(), Ash.Sort.t(), opts :: Keyword.t()) :: t()
  def default_sort(query, sorts, opts \\ []) do
    query = new(query)

    if query.sort == [] do
      sort(query, sorts, opts)
    else
      query
    end
  end

  @doc """
  Adds a resource calculation to the query as a custom calculation with the provided name.

  Example:

      Ash.Query.load_calculation_as(query, :calculation, :some_name, args: %{}, load_through: [:foo])
  """
  def load_calculation_as(query, calc_name, as_name, opts_or_args \\ %{}, opts \\ []) do
    query = new(query)

    {args, opts} =
      if Keyword.keyword?(opts_or_args) do
        {opts_or_args[:args] || %{}, opts_or_args}
      else
        {opts_or_args, opts}
      end

    args = Map.put(args, :as, as_name)

    if resource_calculation = Ash.Resource.Info.calculation(query.resource, calc_name) do
      if opts[:load_through] do
        load_resource_calculation(query, resource_calculation, args)
      else
        load_resource_calculation(query, resource_calculation, args, opts[:load_through])
      end
    else
      add_error(query, "No such calculation: #{inspect(calc_name)}")
    end
  end

  @doc """
  Limits the number of results returned from the query.

  This function sets the maximum number of records that will be returned
  when the query is executed. Useful for pagination and preventing
  large result sets from consuming too much memory.

  ## Examples

      # Limit to 10 results
      iex> MyApp.Post |> Ash.Query.limit(10)
      %Ash.Query{limit: 10, ...}

      # Remove existing limit
      iex> query |> Ash.Query.limit(nil)
      %Ash.Query{limit: nil, ...}

      # Use with other query functions
      iex> MyApp.Post
      ...> |> Ash.Query.filter(published: true)
      ...> |> Ash.Query.sort(:created_at)
      ...> |> Ash.Query.limit(5)
      %Ash.Query{limit: 5, ...}

  ## See also

  - `offset/2` for skipping records (pagination)
  - `page/2` for keyset pagination
  - `sort/3` for ordering results before limiting
  """
  @spec limit(t() | Ash.Resource.t(), nil | integer()) :: t()
  def limit(query, nil), do: new(query)

  def limit(query, limit) when is_integer(limit) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :limit) do
      query
      |> Map.put(:limit, max(0, limit))
    else
      add_error(query, :limit, "Data layer does not support limits")
    end
  end

  def limit(query, limit) do
    add_error(query, :offset, InvalidLimit.exception(limit: limit))
  end

  @doc """
  Skips the first n records in the query results.

  This function is often used for offset-based pagination, allowing you
  to skip a specified number of records from the beginning of the result set.
  Often used together with `limit/2` to implement pagination.

  ## Examples

      # Skip the first 20 records
      iex> MyApp.Post |> Ash.Query.offset(20)
      %Ash.Query{offset: 20, ...}

      # Remove existing offset
      iex> query |> Ash.Query.offset(nil)
      %Ash.Query{offset: 0, ...}

      # Pagination example: page 3 with 10 items per page
      iex> MyApp.Post
      ...> |> Ash.Query.sort(:created_at)
      ...> |> Ash.Query.offset(20)  # Skip first 20 (pages 1-2)
      ...> |> Ash.Query.limit(10)   # Take next 10 (page 3)
      %Ash.Query{offset: 20, limit: 10, ...}

  ## See also

  - `limit/2` for limiting the number of results
  - `page/2` for keyset pagination (more efficient for large datasets)
  - `sort/3` for ordering results before offsetting
  """
  @spec offset(t() | Ash.Resource.t(), nil | integer()) :: t()
  def offset(query, nil), do: new(query)

  def offset(query, offset) when is_integer(offset) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :offset) do
      query
      |> Map.put(:offset, max(0, offset))
    else
      add_error(query, :offset, "Data layer does not support offset")
    end
  end

  def offset(query, offset) do
    query
    |> new()
    |> add_error(:offset, InvalidOffset.exception(offset: offset))
  end

  defp load_relationship(query, rel, statement, opts) do
    loaded =
      Map.update!(query, :load, fn load ->
        if Keyword.has_key?(load, rel.name) do
          Keyword.update!(load, rel.name, &load(&1, statement, opts))
        else
          query =
            case statement do
              %Ash.Query{} = statement ->
                statement

              statement ->
                query =
                  if opts[:strict?] && statement not in [nil, []] do
                    select(new(rel.destination), [])
                  else
                    new(rel.destination)
                  end

                load(query, statement, opts)
            end

          Keyword.put(load, rel.name, query)
        end
      end)

    case loaded.load[rel.name] do
      %{errors: errors} when errors != [] ->
        add_error(loaded, :load, Enum.map(errors, &Ash.Error.set_path(&1, [rel.name])))

      related_query ->
        if Map.get(rel, :manual) &&
             (related_query.limit ||
                (related_query.offset && related_query.offset != 0)) do
          add_error(loaded, :error, [
            Ash.Error.Load.InvalidQuery.exception(
              resource: rel.source,
              relationship: rel.name,
              query: related_query
            )
          ])
        else
          loaded
        end
    end
  end

  @doc false
  def validate_load(query, loads, path \\ []) do
    case do_validate_load(query, loads, path) do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  defp do_validate_load(_query, %Ash.Query{} = load_query, path) do
    case load_query.errors do
      [] ->
        []

      errors ->
        Enum.map(errors, &Ash.Error.set_path(&1, path))
    end
  end

  defp do_validate_load(query, {atom, _} = tuple, path) when is_atom(atom) do
    do_validate_load(query, [tuple], path)
  end

  defp do_validate_load(query, loads, path) when is_list(loads) do
    query = new(query)

    loads
    |> List.wrap()
    |> Enum.flat_map(fn
      {key, value} ->
        case Ash.Resource.Info.relationship(query.resource, key) do
          nil ->
            [
              NoSuchRelationship.exception(
                resource: query.resource,
                relationship: key,
                load_path: Enum.reverse(path)
              )
            ]

          relationship ->
            cond do
              !Ash.Resource.Info.primary_action(relationship.destination, :read) ->
                [
                  NoReadAction.exception(
                    resource: relationship.destination,
                    when: "loading relationship #{relationship.name}"
                  )
                ]

              relationship.type == :many_to_many &&
                  !Ash.Resource.Info.primary_action(relationship.through, :read) ->
                [
                  NoReadAction.exception(
                    resource: relationship.through,
                    when: "loading relationship #{relationship.name}"
                  )
                ]

              true ->
                validate_matching_query_and_continue(
                  value,
                  query.resource,
                  key,
                  path,
                  relationship
                )
            end
        end
    end)
  end

  @doc false
  def do_filter(query, filter, opts \\ [])

  def do_filter(query, %Ash.Filter{} = filter, opts) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :filter) do
      context = %{} |> with_parent_stack(opts) |> with_conflicting_upsert_values(opts)

      new_filter =
        case query.filter do
          nil ->
            Ash.Filter.parse(
              query.resource,
              filter,
              context
            )

          existing_filter ->
            Ash.Filter.add_to_filter(
              existing_filter,
              filter,
              :and,
              query.aggregates,
              query.calculations,
              context
            )
        end

      case new_filter do
        {:ok, filter} ->
          case Ash.Filter.hydrate_refs(
                 filter,
                 %{
                   resource: query.resource,
                   public?: false
                 }
                 |> with_parent_stack(opts)
                 |> with_conflicting_upsert_values(opts)
               ) do
            {:ok, result} ->
              %{query | filter: result}

            {:error, error} ->
              add_error(query, :filter, error)
          end

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  def do_filter(query, nil, _opts), do: new(query)
  def do_filter(query, [], _opts), do: new(query)

  def do_filter(query, statement, opts) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :filter) do
      context =
        %{}
        |> with_parent_stack(opts)
        |> with_conflicting_upsert_values(opts)

      filter =
        if query.filter do
          Ash.Filter.add_to_filter(
            query.filter,
            statement,
            :and,
            query.aggregates,
            query.calculations,
            context
          )
        else
          Ash.Filter.parse(
            query.resource,
            statement,
            context
          )
        end

      case filter do
        {:ok, filter} ->
          case Ash.Filter.hydrate_refs(
                 filter,
                 %{
                   resource: query.resource,
                   public?: false
                 }
                 |> with_parent_stack(opts)
                 |> with_conflicting_upsert_values(opts)
               ) do
            {:ok, result} ->
              %{query | filter: result}

            {:error, error} ->
              add_error(query, :filter, error)
          end

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  defp with_parent_stack(context, opts) do
    if opts[:parent_stack] do
      parent_stack = List.wrap(opts[:parent_stack])

      Map.update(context, :parent_stack, parent_stack, &(parent_stack ++ &1))
    else
      context
    end
  end

  defp with_conflicting_upsert_values(context, opts) do
    case Keyword.fetch(opts, :conflicting_upsert_values) do
      {:ok, values} ->
        Map.put(context, :conflicting_upsert_values, values)

      :error ->
        context
    end
  end

  @doc """
  Lock the query results.

  This must be run while in a transaction, and is not supported by all data layers.
  """
  @spec lock(t() | Ash.Resource.t(), Ash.DataLayer.lock_type()) :: t()
  def lock(query, nil), do: %{query | lock: nil}

  def lock(query, lock_type) do
    query = new(query)

    if Ash.DataLayer.data_layer_can?(query.resource, {:lock, lock_type}) do
      %{query | lock: lock_type}
    else
      add_error(
        query,
        Ash.Error.Query.LockNotSupported.exception(resource: query.resource, lock_type: lock_type)
      )
    end
  end

  @doc """
  Sort the results based on attributes, aggregates or calculations.

  ## Format

  Your sort can be an atom, list of atoms, a keyword list, or a string. When an order is not specified,
  `:asc` is the default. See Sort Orders below for more on the available orders.

  ```elixir
  # sort by name ascending
  Ash.Query.sort(query, :name)

  # sort by name descending
  Ash.Query.sort(query, name: :desc)

  # sort by name descending with nils at the end
  Ash.Query.sort(query, name: :desc_nils_last)

  # sort by name descending, and title ascending
  Ash.Query.sort(query, name: :desc, title: :asc)

  # sort by name ascending
  Ash.Query.sort(query, "name")

  # sort by name descending, and title ascending
  Ash.Query.sort(query, "-name,title")

  # sort by name descending with nils at the end
  Ash.Query.sort(query, "--name")
  ```

  ## Related Fields

  You can refer to related fields using the shorthand of `"rel1.rel2.field"`. For example:

  ```elixir
  # sort by the username of the comment's author.
  Ash.Query.sort(query, "comment.author.username")

  # Use as an atom for keyword lists
  Ash.Query.sort(query, "comment.author.username": :desc)
  ```

  ## Expression Sorts

  You can use the `Ash.Expr.calc/2` macro to sort on expressions:

  ```elixir
  import Ash.Expr

  # Sort on an expression
  Ash.Query.sort(query, calc(count(friends), :desc))

  # Specify a type (required in some cases when we can't determine a type)
  Ash.Query.sort(query, [{calc(fragment("some_sql(?)", field, type: :string), :desc}])
  ```

  ## Sort Strings

  A comma separated list of fields to sort on, each with an optional prefix.

  The prefixes are:

  * "+" - Same as no prefix. Sorts `:asc`.
  * "++" - Sorts `:asc_nils_first`
  * "-" - Sorts `:desc`
  * "--" - Sorts `:desc_nils_last`

  For example

      "foo,-bar,++baz,--buz"

  ## A list of sort strings

  Same prefix rules as above, but provided as a list.

  For example:

      ["foo", "-bar", "++baz", "--buz"]


  ## Calculations

  Calculation inputs can be provided by providing a map. To provide both inputs and an order,
  use a tuple with the first element being the inputs, and the second element being the order.

  ```elixir
  Ash.Query.sort(query, full_name: %{separator: " "})

  Ash.Query.sort(query, full_name: {%{separator: " "}, :asc})
  ```

  ## Sort Orders

  The available orders are:

  - `:asc` - Sort values ascending, with lowest first and highest last, and `nil` values at the end
  - `:desc` - Sort values descending, with highest first and lowest last, and `nil` values at the beginning
  - `:asc_nils_first` - Sort values ascending, with lowest first and highest last, and `nil` values at the beginning
  - `:desc_nils_last` - Sort values descending, with highest first and lowest last, and `nil` values at the end

  ## Examples

  ```elixir
  Ash.Query.sort(query, [:foo, :bar])

  Ash.Query.sort(query, [:foo, bar: :desc])

  Ash.Query.sort(query, [foo: :desc, bar: :asc])
  ```

  See the guide on calculations for more.

  ## Options

  - `prepend?` - set to `true` to put your sort at the front of the list of a sort is already specified
  """
  @spec sort(t() | Ash.Resource.t(), Ash.Sort.t(), opts :: Keyword.t()) :: t()
  def sort(query, sorts, opts \\ []) do
    query = new(query)

    if sorts == [] || sorts == nil do
      query
    else
      case Ash.Actions.Sort.process(query.resource, sorts) do
        {:ok, sorts} ->
          if Ash.DataLayer.data_layer_can?(query.resource, :sort) do
            if opts[:prepend?] && query.sort != [] do
              validated =
                query
                |> Map.put(:sort, [])
                |> sort(sorts)
                |> Map.get(:sort)

              new_sort_input_indices =
                Enum.map(query.sort_input_indices, &(&1 + 1))

              %{query | sort: validated ++ query.sort, sort_input_indices: new_sort_input_indices}
            else
              sorts
              |> List.wrap()
              |> Enum.reduce(query, fn
                {sort, direction}, query ->
                  %{query | sort: query.sort ++ [{sort, direction}]}

                sort, query ->
                  %{query | sort: query.sort ++ [{sort, :asc}]}
              end)
            end
          else
            add_error(query, :sort, "Data layer does not support sorting")
          end

        {:error, error} ->
          Ash.Query.add_error(query, :sort, error)
      end
    end
    |> sequence_sorts()
  end

  @doc """
  Get results distinct on the provided fields.

  Takes a list of fields to distinct on. Each call is additive, so to remove the `distinct` use
  `unset/2`.

  Examples:

  ```
  Ash.Query.distinct(query, [:first_name, :last_name])

  Ash.Query.distinct(query, :email)
  ```
  """
  @spec distinct(t() | Ash.Resource.t(), Ash.Sort.t()) :: t()
  def distinct(query, distincts) do
    query = new(query)
    distincts = List.wrap(distincts)

    if Ash.DataLayer.data_layer_can?(query.resource, :distinct) do
      case Sort.process(query.resource, distincts, query.aggregates, query.context) do
        {:ok, distincts} ->
          %{query | distinct: List.wrap(query.distinct) ++ List.wrap(distincts)}

        {:error, error} ->
          add_error(query, :distinct, error)
      end
    else
      add_error(query, :distinct, "Data layer does not support distincting")
    end
  end

  @doc """
  Set a sort to determine how distinct records are selected.

  If none is set, any sort applied to the query will be used.

  This is useful if you want to control how the `distinct` records
  are selected without affecting (necessarily, it may affect it if
  there is no sort applied) the overall sort of the query
  """
  @spec distinct_sort(t() | Ash.Resource.t(), Ash.Sort.t()) :: t()
  def distinct_sort(query, sorts, opts \\ []) do
    query = new(query)

    if sorts == [] || sorts == nil do
      query
    else
      if Ash.DataLayer.data_layer_can?(query.resource, :distinct_sort) do
        if opts[:prepend?] && query.distinct_sort != [] do
          query_sort = query.distinct_sort

          query
          |> Map.put(:distinct_sort, [])
          |> distinct_sort(sorts)
          |> distinct_sort(query_sort)
        else
          sorts
          |> List.wrap()
          |> Enum.reduce(query, fn
            {sort, direction}, query ->
              %{query | distinct_sort: query.distinct_sort ++ [{sort, direction}]}

            sort, query ->
              %{query | distinct_sort: query.distinct_sort ++ [{sort, :asc}]}
          end)
        end
      else
        add_error(query, :distinct_sort, "Data layer does not support distinct sorting")
      end
    end
  end

  @doc """
  Applies a query to a list of records in memory.

  This function takes a query and applies its filters, sorting, pagination,
  and loading operations to an existing list of records in memory rather than
  querying the data layer. Useful for post-processing records or applying
  query logic to data from multiple sources.

  ## Examples

      # Apply filtering to records in memory
      iex> records = [%MyApp.Post{title: "A", published: true}, %MyApp.Post{title: "B", published: false}]
      iex> query = MyApp.Post |> Ash.Query.filter(published: true)
      iex> Ash.Query.apply_to(query, records)
      {:ok, [%MyApp.Post{title: "A", published: true}]}

      # Apply sorting and limiting
      iex> records = [%MyApp.Post{title: "C", likes: 5}, %MyApp.Post{title: "A", likes: 10}]
      iex> query = MyApp.Post |> Ash.Query.sort(likes: :desc) |> Ash.Query.limit(1)
      iex> Ash.Query.apply_to(query, records)
      {:ok, [%MyApp.Post{title: "A", likes: 10}]}

      # Apply with loading relationships
      iex> records = [%MyApp.Post{id: 1}, %MyApp.Post{id: 2}]
      iex> query = MyApp.Post |> Ash.Query.load(:author)
      iex> Ash.Query.apply_to(query, records, domain: MyApp.Blog)
      {:ok, [%MyApp.Post{id: 1, author: %MyApp.User{...}}, ...]}

  ## Options

    * `domain` - The domain to use for loading relationships
    * `actor` - The actor for authorization during loading
    * `tenant` - The tenant for multitenant operations
    * `parent` - Parent context for nested operations

  ## See also

  - `Ash.read/2` for querying the data layer directly
  - `load/3` for configuring relationship loading
  - `filter/2` for adding filter conditions
  """
  @spec apply_to(t(), records :: list(Ash.Resource.record()), opts :: Keyword.t()) ::
          {:ok, list(Ash.Resource.record())}
  def apply_to(query, records, opts \\ []) do
    domain =
      query.domain || Ash.Resource.Info.domain(query.resource) || opts[:domain] ||
        Ash.Actions.Helpers.maybe_embedded_domain(query.resource) ||
        raise ArgumentError,
              "Could not determine domain for #{inspect(query)}, please provide the `:domain` option."

    with {:ok, records} <-
           Ash.Filter.Runtime.filter_matches(domain, records, query.filter,
             parent: opts[:parent],
             actor: opts[:actor] || query.context[:private][:actor],
             tenant: opts[:tenant] || query.tenant
           ),
         records <- Sort.runtime_sort(records, query.distinct_sort || query.sort, domain: domain),
         records <- Sort.runtime_distinct(records, query.distinct, domain: domain),
         records <- Sort.runtime_sort(records, query.sort, domain: domain),
         records <- Enum.drop(records, query.offset),
         records <- do_limit(records, query.limit),
         {:ok, records} <- Ash.load(records, query, domain: domain, reuse_values?: true) do
      {:ok, records}
    else
      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp do_limit(records, nil), do: records
  defp do_limit(records, limit), do: Enum.take(records, limit)

  @doc """
  Removes specified keys from the query, resetting them to their default values.

  This function allows you to "unset" or reset parts of a query back to their
  initial state. Useful when you want to remove filters, sorts, loads, or other
  query modifications while keeping the rest of the query intact.

  ## Examples

      # Remove multiple query aspects at once
      iex> query = MyApp.Post
      ...> |> Ash.Query.filter(published: true)
      ...> |> Ash.Query.sort(:created_at)
      ...> |> Ash.Query.limit(10)
      iex> Ash.Query.unset(query, [:filter, :sort, :limit])
      %Ash.Query{filter: nil, sort: [], limit: nil, ...}

      # Remove just the sort from a query
      iex> query = MyApp.Post |> Ash.Query.sort([:title, :created_at])
      iex> Ash.Query.unset(query, :sort)
      %Ash.Query{sort: [], ...}

      # Remove load statements
      iex> query = MyApp.Post |> Ash.Query.load([:author, :comments])
      iex> Ash.Query.unset(query, :load)
      %Ash.Query{load: [], ...}

      # Reset pagination settings
      iex> query = MyApp.Post |> Ash.Query.limit(20) |> Ash.Query.offset(10)
      iex> Ash.Query.unset(query, [:limit, :offset])
      %Ash.Query{limit: nil, offset: 0, ...}

  ## See also

  - `new/2` for creating fresh queries
  - `select/3`, `filter/2`, `sort/3` for building queries
  """
  @spec unset(Ash.Resource.t() | t(), atom | [atom]) :: t()
  def unset(query, keys) when is_list(keys) do
    query = new(query)

    new = new(query.resource)

    keys
    |> Enum.reduce(query, fn key, query ->
      do_unset(query, key, new)
    end)
  end

  def unset(query, key) do
    new = new(query.resource)
    do_unset(query, key, new)

    query
    |> new()
    |> struct([{key, Map.get(new, key)}])
  end

  defp do_unset(query, key, _new) when key in [:domain, :resource] do
    query
  end

  defp do_unset(query, :load, new) do
    query = unset(query, [:calculations, :aggregates, :load_through])

    struct(query, [{:load, Map.get(new, :load)}])
  end

  defp do_unset(query, :sort, _new) do
    %{query | sort: [], sort_input_indices: []}
  end

  defp do_unset(query, key, new) do
    struct(query, [{key, Map.get(new, key)}])
  end

  @doc "Return the underlying data layer query for an ash query"
  def data_layer_query(ash_query, opts \\ [])

  def data_layer_query(%{errors: errors}, _opts) when errors not in [[], nil] do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def data_layer_query(%{resource: resource, domain: domain} = ash_query, opts) do
    context =
      ash_query.context
      |> Map.put(:action, ash_query.action)
      |> Map.put_new(:private, %{})
      |> put_in([:private, :tenant], ash_query.tenant)
      |> Map.put_new(:data_layer, %{})

    context =
      if opts[:previous_combination] do
        Map.update!(
          context,
          :data_layer,
          &Map.put(
            &1,
            :previous_combination,
            opts[:previous_combination] &&
              Ash.DataLayer.combination_acc(opts[:previous_combination], resource)
          )
        )
      else
        context
      end

    with {:ok, query, new_context} <- initial_data_layer_query(ash_query, domain, opts),
         {:ok, query} <-
           Ash.DataLayer.set_context(
             resource,
             query,
             Map.update!(
               context,
               :data_layer,
               &Map.merge(&1 || %{}, new_context)
             )
           ),
         {:ok, query} <- add_tenant(query, ash_query),
         {:ok, query} <-
           Ash.DataLayer.select(query, ash_query.select, ash_query.resource),
         {:ok, query} <- Ash.DataLayer.sort(query, ash_query.sort, resource),
         {:ok, query} <- Ash.DataLayer.distinct_sort(query, ash_query.distinct_sort, resource),
         {:ok, query} <-
           Ash.DataLayer.add_aggregates(
             query,
             Map.values(ash_query.aggregates),
             ash_query.resource
           ),
         {:ok, query} <- maybe_filter(query, ash_query, opts),
         {:ok, query} <-
           Ash.DataLayer.add_calculations(
             query,
             opts[:data_layer_calculations] || [],
             ash_query.resource
           ),
         {:ok, query} <- Ash.DataLayer.distinct(query, ash_query.distinct, resource),
         {:ok, query} <- Ash.DataLayer.limit(query, ash_query.limit, resource),
         {:ok, query} <- Ash.DataLayer.offset(query, ash_query.offset, resource),
         {:ok, query} <- Ash.DataLayer.lock(query, ash_query.lock, resource),
         {:ok, query} <- maybe_return_query(query, resource, opts) do
      if opts[:no_modify?] || !ash_query.action || !ash_query.action.modify_query do
        {:ok, query}
      else
        case ash_query.action.modify_query do
          {m, f, a} ->
            apply(m, f, a ++ [ash_query, query])

          modify_query when is_function(modify_query, 2) ->
            modify_query.(ash_query, query)
        end
      end
    else
      {:error, error} -> {:error, error}
    end
  end

  defp initial_data_layer_query(ash_query, domain, opts) do
    cond do
      opts[:initial_query] ->
        {:ok, opts[:initial_query], %{}}

      ash_query.combination_of != [] ->
        combination = hd(ash_query.combination_of)

        default_select =
          MapSet.to_list(
            Ash.Resource.Info.selected_by_default_attribute_names(ash_query.resource)
          )

        case combination_queries(ash_query) do
          {:ok, combinations, previous} ->
            with {:ok, query} <-
                   Ash.DataLayer.combination_of(combinations, ash_query.resource, domain) do
              {:ok, query,
               %{
                 previous_combination:
                   previous && Ash.DataLayer.combination_acc(previous, ash_query.resource),
                 combination_of_queries?: true,
                 combination_fieldset:
                   Enum.uniq(
                     (combination.select || default_select) ++ Map.keys(combination.calculations)
                   )
               }}
            end

          {:error, error} ->
            {:error, error}
        end

      true ->
        {:ok, opts[:initial_query] || Ash.DataLayer.resource_to_query(ash_query.resource, domain),
         %{}}
    end
  end

  defp combination_queries(query) do
    base_query = Ash.Query.new(query.resource)

    Enum.reduce_while(
      query.combination_of,
      {:ok, [], nil},
      fn combination, {:ok, combinations, previous} ->
        calculations =
          Enum.map(combination.calculations, fn {name, calc} ->
            {%{calc | name: name, load: nil}, calc.module.expression(calc.opts, calc.context)}
          end)

        base_query
        |> Ash.Query.set_tenant(query.tenant)
        |> limit(combination.limit)
        |> offset(combination.offset)
        |> do_filter(combination.filter)
        |> sort(combination.sort)
        |> select(
          combination.select ||
            MapSet.to_list(Ash.Resource.Info.selected_by_default_attribute_names(query.resource))
        )
        |> Ash.Query.set_context(query.context)
        |> Ash.Query.set_context(%{data_layer: %{combination_query?: true}})
        |> then(fn
          %{valid?: true} = combination_query ->
            case data_layer_query(combination_query,
                   previous_combination: previous,
                   data_layer_calculations: calculations
                 ) do
              {:ok, combination_query} ->
                {:cont,
                 {:ok, [{combination.type, combination_query} | combinations], combination_query}}

              {:error, error} ->
                {:halt, {:error, error}}
            end

          %{valid?: false, errors: errors} ->
            {:halt, {:error, errors}}
        end)
      end
    )
    |> then(fn
      {:ok, unions, previous} ->
        {:ok, Enum.reverse(unions), previous}

      {:error, error} ->
        {:error, error}
    end)
  end

  defp maybe_return_query(query, resource, opts) do
    if Keyword.get(opts, :run_return_query?, true) do
      Ash.DataLayer.return_query(query, resource)
    else
      {:ok, query}
    end
  end

  defp add_tenant(query, ash_query) do
    with :context <- Ash.Resource.Info.multitenancy_strategy(ash_query.resource),
         tenant when not is_nil(tenant) <- ash_query.to_tenant,
         {:ok, query} <- Ash.DataLayer.set_tenant(ash_query.resource, query, ash_query.to_tenant) do
      {:ok, query}
    else
      {:error, error} -> {:error, error}
      _ -> {:ok, query}
    end
  end

  @doc """
  Add an error to the errors list and mark the query as invalid.

  See `Ash.Error.to_ash_error/3` for more on supported values for `error`

  ## Inconsistencies
  The `path` argument is the second argument here, but the third argument
  in `Ash.ActionInput.add_error/2` and `Ash.Changeset.add_error/2`.
  This will be fixed in 4.0.
  """
  @spec add_error(t(), path :: Ash.Error.path_input(), Ash.Error.error_input()) :: t()
  @spec add_error(t(), Ash.Error.error_input()) :: t()

  def add_error(query, path \\ [], error)

  def add_error(query, _path, []) do
    query
  end

  def add_error(query, path, errors) when is_list(errors) do
    if Keyword.keyword?(errors) do
      error =
        errors
        |> to_query_error()
        |> Ash.Error.set_path(path)

      add_error(query, error)
    else
      Enum.reduce(errors, query, &add_error(&2, path, &1))
    end
  end

  def add_error(query, path, error) do
    path = List.wrap(path)
    query = new(query)

    error =
      error
      |> Ash.Error.to_ash_error()
      |> Ash.Error.set_path(path)

    %{query | errors: [error | query.errors], valid?: false}
  end

  defp to_query_error(keyword) do
    error =
      if keyword[:field] do
        InvalidArgument.exception(
          field: keyword[:field],
          message: keyword[:message],
          value: keyword[:value],
          vars: keyword
        )
      else
        InvalidQuery.exception(
          fields: keyword[:fields] || [],
          message: keyword[:message],
          value: keyword[:value],
          vars: keyword
        )
      end

    if keyword[:path] do
      Ash.Error.set_path(error, keyword[:path])
    else
      error
    end
  end

  defp validate_matching_query_and_continue(value, resource, key, path, relationship) do
    %{destination: relationship_resource} = relationship

    case value do
      %__MODULE__{resource: query_resource} = destination_query
      when query_resource != relationship_resource ->
        [
          Ash.Error.Load.InvalidQuery.exception(
            resource: resource,
            relationship: key,
            query: destination_query,
            load_path: Enum.reverse(path)
          )
        ]

      %__MODULE__{} = destination_query ->
        if Map.get(relationship, :manual) &&
             (destination_query.limit ||
                (destination_query.offset && destination_query.offset != 0)) do
          [
            Ash.Error.Load.InvalidQuery.exception(
              resource: resource,
              relationship: key,
              query: destination_query,
              load_path: Enum.reverse(path)
            )
          ]
        else
          do_validate_load(relationship.destination, destination_query, [key | path])
        end

      other ->
        do_validate_load(relationship.destination, other, [key | path])
    end
  end

  defp maybe_filter(query, %{filter: nil}, _) do
    {:ok, query}
  end

  defp maybe_filter(query, ash_query, opts) do
    case Ash.DataLayer.filter(query, ash_query.filter, ash_query.resource) do
      {:ok, filtered} ->
        if Keyword.get(opts, :only_validate_filter?, false) do
          {:ok, query}
        else
          {:ok, filtered}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp set_load_domain(nil, _), do: nil
  defp set_load_domain([], _), do: []

  defp set_load_domain(%__MODULE__{} = query, domain) do
    set_domain(query, domain)
  end

  defp set_load_domain(loads, domain) do
    Enum.map(loads, fn {key, further} ->
      {key, set_load_domain(further, domain)}
    end)
  end

  defp merge_load(
         %__MODULE__{
           resource: resource,
           load: left_loads,
           timeout: left_timeout,
           calculations: left_calculations,
           aggregates: left_aggregates,
           tenant: left_tenant,
           select: left_select
         },
         %__MODULE__{
           load: right_loads,
           timeout: right_timeout,
           aggregates: right_aggregates,
           calculations: right_calculations,
           select: right_select
         } =
           query,
         opts
       ) do
    select =
      if is_nil(left_select) or is_nil(right_select) do
        Enum.to_list(Ash.Resource.Info.selected_by_default_attribute_names(resource))
      else
        Enum.uniq(left_select ++ right_select)
      end

    %{
      query
      | load: left_loads,
        calculations: Map.merge(left_calculations, right_calculations),
        aggregates: Map.merge(left_aggregates, right_aggregates),
        timeout: left_timeout || right_timeout,
        select: select
    }
    |> merge_load(right_loads, opts)
    |> set_tenant(query.tenant || left_tenant)
  end

  defp merge_load(%__MODULE__{} = query, right, opts) do
    load(query, right, opts)
  end
end
