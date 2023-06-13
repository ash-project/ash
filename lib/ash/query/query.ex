defmodule Ash.Query do
  @moduledoc """
  Utilities around constructing/manipulating ash queries.

  Ash queries are used for read actions and loads, and ultimately
  map to queries to a resource's data layer.

  Queries are run by calling `read` on an API that contains the resource in question

  Examples:

  ```elixir
  MyApp.Post
  |> Ash.Query.filter(likes > 10)
  |> Ash.Query.sort([:title])
  |> MyApp.Api.read!()

  MyApp.Author
  |> Ash.Query.aggregate(:published_post_count, :posts, query: [filter: [published: true]])
  |> Ash.Query.sort(published_post_count: :desc)
  |> Ash.Query.limit(10)
  |> MyApp.Api.read!()

  MyApp.Author
  |> Ash.Query.load([:post_count, :comment_count])
  |> Ash.Query.load(posts: [:comments])
  |> MyApp.Api.read!()
  ```
  """

  defstruct [
    :__validated_for_action__,
    :action,
    :api,
    :distinct,
    :filter,
    :resource,
    :tenant,
    :timeout,
    :lock,
    load_through: %{},
    action_failed?: false,
    after_action: [],
    aggregates: %{},
    arguments: %{},
    before_action: [],
    calculations: %{},
    context: %{},
    errors: [],
    limit: nil,
    load: [],
    offset: 0,
    params: %{},
    phase: :preparing,
    select: nil,
    sort: [],
    valid?: true
  ]

  @type t :: %__MODULE__{
          __validated_for_action__: atom | nil,
          action: Ash.Resource.Actions.Read.t() | nil,
          api: module | nil,
          distinct: [atom],
          filter: Ash.Filter.t() | nil,
          resource: module,
          tenant: any,
          timeout: pos_integer() | nil,
          action_failed?: boolean,
          after_action: [
            (t, [Ash.Resource.record()] ->
               {:ok, [Ash.Resource.record()]}
               | {:ok, [Ash.Resource.record()], [Ash.Notifier.Notification.t()]}
               | {:error, any})
          ],
          aggregates: %{optional(atom) => Ash.Filter.t()},
          arguments: %{optional(atom) => any},
          before_action: [(t -> t)],
          calculations: %{optional(atom) => :wat},
          context: map,
          errors: [Ash.Error.t()],
          limit: nil | non_neg_integer(),
          load: keyword(keyword),
          offset: non_neg_integer(),
          params: %{optional(atom | binary) => any},
          phase: :preparing | :before_action | :after_action | :executing,
          select: nil | [atom],
          sort: [atom | {atom, :asc | :desc}],
          valid?: boolean
        }

  alias Ash.Actions.Sort

  alias Ash.Error.Invalid.TimeoutNotSupported

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidArgument,
    InvalidCalculationArgument,
    InvalidLimit,
    InvalidOffset,
    NoReadAction,
    ReadActionRequiresActor,
    Required
  }

  alias Ash.Error.Load.{InvalidQuery, NoSuchRelationship}
  alias Ash.Query.{Aggregate, Calculation}

  require Ash.Tracer
  import Ash.Filter.TemplateHelpers, only: [expr?: 1]

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

      sort? = query.sort != []
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

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", inspect(query.resource)),
          or_empty(concat("tenant: ", to_doc(query.tenant, opts)), tenant?),
          arguments(query, opts),
          or_empty(concat("filter: ", to_doc(query.filter, opts)), filter?),
          or_empty(concat("sort: ", to_doc(query.sort, opts)), sort?),
          or_empty(concat("limit: ", to_doc(query.limit, opts)), limit?),
          or_empty(concat("offset: ", to_doc(query.offset, opts)), offset?),
          or_empty(concat("load: ", to_doc(query.load, opts)), load?),
          or_empty(concat("aggregates: ", to_doc(query.aggregates, opts)), aggregates?),
          or_empty(concat("calculations: ", to_doc(query.calculations, opts)), calculations?),
          or_empty(concat("errors: ", to_doc(query.errors, opts)), errors?),
          or_empty(concat("select: ", to_doc(query.select, opts)), select?),
          or_empty(concat("distinct: ", to_doc(query.distinct, opts)), distinct?),
          or_empty(concat("lock: ", to_doc(query.lock, opts)), lock?)
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
          arg_string =
            query.action.arguments
            |> Map.new(fn argument ->
              if argument.sensitive? do
                {argument.name, "**redacted**"}
              else
                {argument.name, Ash.Query.get_argument(query, argument.name)}
              end
            end)
            |> to_doc(opts)

          concat(["arguments: ", arg_string])
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

      if query.__validated_for_action__ && !query.context[:private][:in_before_action?] do
        IO.warn("""
        Query has already been validated for action #{inspect(query.__validated_for_action__)}.

        For safety, we prevent any changes after that point because they will bypass validations or other action logic.
        However, you should prefer a pattern like the below, which makes any custom modifications *before* calling the action.

          Resource
          |> Ash.Query.new()
          |> Ash.Query.#{unquote(function)}(...)
          |> Ash.Query.for_read(...)
        """)
      end
    end
  end

  @doc """
  Attach a filter statement to the query.

  The filter is applied as an "and" to any filters currently on the query.
  For more information on writing filters, see: `Ash.Filter`.
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
      require Ash.Expr
      expr = Ash.Expr.do_expr(expression)

      quote location: :keep do
        Ash.Query.do_filter(unquote(query), List.wrap(unquote(expr)))
      end
    end
  end

  @doc "Create a new query"
  def new(resource, api \\ nil, opts \\ [])
  def new(%__MODULE__{} = query, _, _opts), do: query

  def new(resource, api, opts) when is_atom(resource) do
    query = %__MODULE__{
      api: api,
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
              |> Ash.Filter.parse!(filter, query.aggregates, query.calculations, query.context)
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

    set_context(query, context)
  end

  @for_read_opts [
    actor: [
      type: :any,
      doc:
        "set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    authorize?: [
      type: :boolean,
      doc:
        "set authorize?, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    authorize?: [
      type: :boolean,
      doc:
        "set tracer, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
    ],
    tracer: [
      type: :atom,
      doc:
        "A tracer to use. Will be carried over to the action. For more information see `Ash.Tracer`."
    ],
    tenant: [
      type: :any,
      doc: "set the tenant on the query"
    ]
  ]

  def for_read_opts, do: @for_read_opts

  @doc """
  Creates a query for a given read action and prepares it.

  Multitenancy is *not* validated until an action is called. This allows you to avoid specifying a tenant until just before calling
  the api action.

  ### Arguments
  Provide a map or keyword list of arguments for the read action

  ### Opts

  #{Spark.OptionsHelpers.docs(@for_read_opts)}

  """
  def for_read(query, action_name, args \\ %{}, opts \\ []) do
    query = to_query(query)

    {query, opts} = Ash.Actions.Helpers.add_process_context(query.api, query, opts)

    query =
      query
      |> Map.put(:params, Map.merge(query.params, Map.new(args)))
      |> set_context(Keyword.get(opts, :context, %{}))

    action = Ash.Resource.Info.action(query.resource, action_name, :read)

    if action do
      name = "query:" <> Ash.Resource.Info.trace_name(query.resource) <> ":#{action_name}"

      Ash.Tracer.span :query,
                      name,
                      opts[:tracer] do
        Ash.Tracer.telemetry_span [:ash, :query], %{
          resource_short_name: Ash.Resource.Info.short_name(query.resource)
        } do
          metadata = %{
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            resource: query.resource,
            actor: opts[:actor],
            tenant: opts[:tenant],
            action: action.name,
            authorize?: opts[:authorize?]
          }

          Ash.Tracer.set_metadata(opts[:tracer], :query, metadata)

          query
          |> Map.put(:action, action)
          |> reset_arguments()
          |> timeout(query.timeout || opts[:timeout])
          |> set_actor(opts)
          |> set_authorize?(opts)
          |> set_tracer(opts)
          |> Ash.Query.set_tenant(opts[:tenant] || query.tenant)
          |> cast_params(action, args)
          |> set_argument_defaults(action)
          |> require_arguments(action)
          |> run_preparations(action, opts[:actor], opts[:authorize?], opts[:tracer], metadata)
          |> add_action_filters(action, opts[:actor])
          |> Map.put(:__validated_for_action__, action_name)
        end
      end
    else
      add_error(query, :action, "No such action #{action_name}")
    end
  end

  def timeout(query, timeout) do
    query = to_query(query)

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

  defp require_arguments(query, action) do
    action.arguments
    |> Enum.filter(&(&1.allow_nil? == false))
    |> Enum.reduce(query, fn argument, query ->
      case fetch_argument(query, argument.name) do
        {:ok, value} when not is_nil(value) ->
          query

        _ ->
          add_error(
            query,
            Required.exception(
              resource: query.resource,
              field: argument.name,
              type: :argument
            )
          )
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

  defp cast_params(query, action, args) do
    Enum.reduce(args, query, fn {name, value}, query ->
      if has_argument?(action, name) do
        set_argument(query, name, value)
      else
        query
      end
    end)
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.private? == false && &1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(&1.private? == false && to_string(&1.name) == name))
  end

  defp run_preparations(query, action, actor, authorize?, tracer, metadata) do
    query.resource
    |> Ash.Resource.Info.preparations()
    |> Enum.concat(action.preparations || [])
    |> Enum.reduce_while(query, fn %{preparation: {module, opts}}, query ->
      Ash.Tracer.span :preparation, "prepare: #{inspect(module)}", tracer do
        Ash.Tracer.telemetry_span [:ash, :preparation], %{
          resource_short_name: Ash.Resource.Info.short_name(query.resource),
          preparation: inspect(module)
        } do
          Ash.Tracer.set_metadata(opts[:tracer], :preparation, metadata)

          case module.init(opts) do
            {:ok, opts} ->
              opts =
                Ash.Filter.build_filter_from_template(
                  opts,
                  actor,
                  query.arguments,
                  query.context
                )

              case module.prepare(query, opts, %{
                     actor: actor,
                     authorize?: authorize?,
                     tracer: tracer
                   }) do
                %__MODULE__{} = prepared ->
                  {:cont, prepared}

                other ->
                  raise """
                  Invalid value returned from #{inspect(module)}.prepare/3

                  A query must be returned, but the following was received instead:

                  #{inspect(other)}
                  """
              end

            {:error, error} ->
              {:halt, Ash.Query.add_error(query, error)}
          end
        end
      end
    end)
  end

  @spec before_action(
          t(),
          (t() -> t() | {t(), list(Ash.Notifier.Notification.t())})
        ) ::
          t()
  def before_action(query, func) do
    query = to_query(query)
    %{query | before_action: [func | query.before_action]}
  end

  @spec after_action(
          t(),
          (t(), [Ash.Resource.record()] ->
             {:ok, [Ash.Resource.record()]}
             | {:ok, [Ash.Resource.record()], list(Ash.Notifier.Notification.t())}
             | {:error, term})
        ) :: t()
  def after_action(query, func) do
    query = to_query(query)
    %{query | after_action: [func | query.after_action]}
  end

  defp add_action_filters(query, %{filter: nil}, _actor), do: query

  defp add_action_filters(query, action, actor) do
    if Ash.Filter.template_references_actor?(action.filter) and is_nil(actor) do
      Ash.Query.add_error(query, ReadActionRequiresActor.exception([]))
    else
      built_filter =
        Ash.Filter.build_filter_from_template(
          action.filter,
          actor,
          query.arguments,
          query.context
        )

      do_filter(query, built_filter)
    end
  end

  @doc "Returns true if the value is one of the expression structs."
  @deprecated "use Ash.Filter.TemplateHelpers.expr?/1"
  def is_expr?(value), do: expr?(value)

  @doc """
  Creates an Ash expression for evaluation later.
  """
  defmacro expr(body) do
    quote location: :keep do
      require Ash.Expr
      Ash.Expr.expr(unquote(body))
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
  """
  def select(query, fields, opts \\ []) do
    query = to_query(query)
    fields = List.wrap(fields)

    {fields, non_existent} =
      Enum.split_with(fields, &Ash.Resource.Info.attribute(query.resource, &1))

    query =
      Enum.reduce(non_existent, query, fn field, query ->
        Ash.Query.add_error(
          query,
          Ash.Error.Query.NoSuchAttribute.exception(resource: query.resource, name: field)
        )
      end)

    always_select =
      query.resource
      |> Ash.Resource.Info.attributes()
      |> Enum.filter(& &1.always_select?)
      |> Enum.map(& &1.name)

    if opts[:replace?] do
      %{
        query
        | select:
            Enum.uniq(fields ++ Ash.Resource.Info.primary_key(query.resource) ++ always_select)
      }
    else
      %{
        query
        | select:
            Enum.uniq(
              fields ++
                (query.select || []) ++
                Ash.Resource.Info.primary_key(query.resource) ++ always_select
            )
      }
    end
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

      Ash.SatSolver.strict_filter_subset(left_filter, right_filter)
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
  Determines if the provided expression would return data that is a suprset of the data returned by the filter on the query.

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
      Ash.SatSolver.strict_filter_subset(left_filter, right_filter)
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

  See `select/2` for more.
  """
  def ensure_selected(query, fields) do
    query = to_query(query)

    if query.select do
      Ash.Query.select(query, List.wrap(fields))
    else
      to_select =
        query.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      Ash.Query.select(query, to_select)
    end
  end

  @doc """
  Returns a list of attributes, aggregates, relationships, and calculations that are being loaded

  Provide a list of field types to narrow down the returned results.
  """
  def accessing(query, types \\ [:attributes, :relationships, :calculations, :attributes]) do
    [
      {:aggregates, Ash.Resource.Info.aggregates(query)},
      {:relationships, Ash.Resource.Info.relationships(query)},
      {:calculations, Ash.Resource.Info.calculations(query)},
      {:attributes, Ash.Resource.Info.attributes(query)}
    ]
    |> Stream.flat_map(fn {type, fields} ->
      if type in types do
        fields
      else
        []
      end
    end)
    |> Stream.map(& &1.name)
    |> Enum.filter(&loading?(query, &1))
  end

  @doc """
  Ensure the the specified attributes are `nil` in the query results.
  """
  def deselect(query, fields) do
    query = to_query(query)

    select =
      if query.select do
        query.select
      else
        query.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)
      end

    select = select -- List.wrap(fields)

    select(query, select, replace?: true)
  end

  def selecting?(query, field) do
    case query.select do
      nil ->
        not is_nil(Ash.Resource.Info.attribute(query.resource, field))

      select ->
        if field in select do
          true
        else
          attribute = Ash.Resource.Info.attribute(query.resource, field)

          attribute && (attribute.primary_key? || attribute.private?)
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
    item in List.wrap(query.select) ||
      Keyword.has_key?(query.load || [], item) ||
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
    Map.update!(query, :load_through, fn load_through ->
      load_through
      |> Map.put_new(type, %{})
      |> Map.update!(type, fn loads ->
        Map.update(loads, name, load, &(List.wrap(&1) ++ load))
      end)
    end)
  end

  @doc """
  Loads relationships, calculations, or aggregates on the resource.

  Currently, loading attributes has no effects, as all attributes are returned.
  Before long, we will have the default list to load as the attributes, but if you say
  `load(query, [:attribute1])`, that will be the only field filled in. This will let
  data layers make more intelligent "select" statements as well.


  ```elixir
  # Loading nested relationships
  Ash.Query.load(query, [comments: [:author, :ratings]])

  # Loading relationships with a query
  Ash.Query.load(query, [comments: [author: author_query]])
  ```
  """
  @spec load(
          t() | Ash.Resource.t(),
          atom
          | Ash.Query.Calculation.t()
          | list(atom | Ash.Query.Calculation.t())
          | list({atom | Ash.Query.Calculation.t(), term})
        ) ::
          t()
  def load(query, fields) when not is_list(fields) do
    load(query, List.wrap(fields))
  end

  def load(query, fields) do
    query = to_query(query)

    Enum.reduce(fields, query, fn
      {field, %__MODULE__{} = nested}, query ->
        load_relationship(query, [{field, nested}])

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
            nested_query = load(rel.destination, rest)

            load_relationship(query, [{field, nested_query}])

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
        do_load(query, field)
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

      {module, opts} = resource_calculation.calculation

      with {:ok, args} <- validate_calculation_arguments(resource_calculation, args),
           {:ok, calculation} <-
             Calculation.new(
               name,
               module,
               opts,
               {resource_calculation.type, resource_calculation.constraints},
               Map.put(args, :context, query.context),
               resource_calculation.filterable?,
               resource_calculation.load
             ) do
        calculation =
          select_and_load_calc(
            resource_calculation,
            %{calculation | load: load, calc_name: resource_calculation.name},
            query
          )

        query = Map.update!(query, :calculations, &Map.put(&1, name, calculation))

        if load_through do
          load_through(query, :calculation, name, load_through)
        else
          query
        end
      else
        {:error, error} ->
          add_error(query, :load, error)
      end
    else
      Ash.Error.Query.InvalidLoad.exception(load: {resource_calculation.name, args})
    end
  end

  @doc false
  def select_and_load_calc(resource_calculation, calculation, query) do
    module = calculation.module
    opts = calculation.opts

    resource_calculation_select =
      if resource_calculation do
        List.wrap(resource_calculation.select)
      else
        []
      end

    resource_calculation_load =
      if resource_calculation do
        List.wrap(resource_calculation.load)
      else
        []
      end

    fields_to_select =
      resource_calculation_select
      |> Enum.concat(module.select(query, opts, calculation.context) || [])
      |> Enum.uniq()
      |> Enum.filter(&Ash.Resource.Info.attribute(query.resource, &1))

    loads =
      module.load(
        query,
        opts,
        Map.put(calculation.context, :context, query.context)
      )
      |> Ash.Actions.Helpers.validate_calculation_load!(module)
      |> Enum.concat(resource_calculation_load)
      |> reify_calculations(query)

    %{calculation | select: fields_to_select, required_loads: loads}
  end

  @doc false
  def reify_calculations(loads, query) do
    loads
    |> List.wrap()
    |> Enum.map(fn
      {load, args} ->
        if resource_calculation = Ash.Resource.Info.calculation(query.resource, load) do
          case resource_calc_to_calc(query, load, resource_calculation, args) do
            {:error, _} ->
              {load, args}

            {:ok, calc} ->
              calc
          end
        else
          if relationship = Ash.Resource.Info.relationship(query.resource, load) do
            related_query = new(relationship.destination)
            {load, reify_calculations(args, related_query)}
          else
            {load, args}
          end
        end

      load ->
        if resource_calculation = Ash.Resource.Info.calculation(query.resource, load) do
          case resource_calc_to_calc(query, load, resource_calculation) do
            {:error, _} ->
              load

            {:ok, calc} ->
              calc
          end
        else
          if relationship = Ash.Resource.Info.relationship(query.resource, load) do
            {load, relationship.destination |> new() |> set_tenant(query.tenant)}
          else
            load
          end
        end
    end)
    |> case do
      [%Ash.Query{} = query] -> query
      other -> other
    end
  end

  @doc false
  def resource_calc_to_calc(query, name, resource_calculation, args \\ %{}) do
    with %{calculation: {module, opts}} <- resource_calculation,
         {:ok, args} <- validate_calculation_arguments(resource_calculation, args),
         {:ok, calculation} <-
           Calculation.new(
             resource_calculation.name,
             module,
             opts,
             {resource_calculation.type, resource_calculation.constraints},
             Map.put(args, :context, query.context),
             resource_calculation.filterable?,
             resource_calculation.load
           ) do
      {:ok,
       select_and_load_calc(
         resource_calculation,
         %{calculation | load: name, calc_name: resource_calculation.name},
         query
       )}
    end
  end

  defp do_load(query, field) do
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

      Ash.Resource.Info.attribute(query.resource, field) ->
        ensure_selected(query, field)

      Ash.Resource.Info.relationship(query.resource, field) ->
        load_relationship(query, field)

      aggregate = Ash.Resource.Info.aggregate(query.resource, field) ->
        related = Ash.Resource.Info.related(query.resource, aggregate.relationship_path)

        read_action =
          aggregate.read_action || Ash.Resource.Info.primary_action!(related, :read).name

        with {:can?, true} <-
               {:can?,
                Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, aggregate.kind})},
             %{valid?: true} = aggregate_query <-
               for_read(related, read_action),
             %{valid?: true} = aggregate_query <-
               build(aggregate_query, filter: aggregate.filter, sort: aggregate.sort),
             {:ok, query_aggregate} <-
               Aggregate.new(
                 query.resource,
                 aggregate.name,
                 aggregate.kind,
                 path: aggregate.relationship_path,
                 query: aggregate_query,
                 field: aggregate.field,
                 default: aggregate.default,
                 filterable?: aggregate.filterable?,
                 type: aggregate.type,
                 constraints: aggregate.constraints,
                 implementation: aggregate.implementation,
                 uniq?: aggregate.uniq?,
                 read_action: aggregate.read_action,
                 authorize?: aggregate.authorize?
               ) do
          query_aggregate = %{query_aggregate | load: field}
          new_aggregates = Map.put(query.aggregates, aggregate.name, query_aggregate)

          %{query | aggregates: new_aggregates}
        else
          %{errors: errors} ->
            add_error(
              query,
              :aggregates,
              Ash.Error.to_ash_error(errors, nil,
                error_context: "Loading aggregate: #{inspect(field)} for query: #{inspect(query)}"
              )
            )

          {:error, error} ->
            add_error(
              query,
              :aggregates,
              Ash.Error.to_ash_error(error, nil,
                error_context: "Loading aggregate: #{inspect(field)} for query: #{inspect(query)}"
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
  def validate_calculation_arguments(calculation, args) do
    args =
      if Keyword.keyword?(args) do
        Map.new(args)
      else
        args
      end

    has_one_expr? = Enum.any?(args, fn {_, value} -> expr?(value) end)

    Enum.reduce_while(calculation.arguments, {:ok, %{}}, fn argument, {:ok, arg_values} ->
      value =
        default(
          Map.get(args, argument.name, Map.get(args, to_string(argument.name))),
          argument.default
        )

      cond do
        expr?(value) && argument.allow_expr? ->
          {:cont,
           {:ok,
            Map.put(
              arg_values,
              argument.name,
              expr(type(^value, ^argument.type, ^argument.constraints))
            )}}

        expr?(value) ->
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

        is_nil(value) ->
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
                expr(type(^value, ^argument.type, ^argument.constraints))
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
                  expr(type(^casted, ^argument.type, ^argument.constraints))
                )}}
            else
              {:cont, {:ok, Map.put(arg_values, argument.name, casted)}}
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
  end

  defp default(nil, {module, function, args}), do: apply(module, function, args)
  defp default(nil, value) when is_function(value, 0), do: value.()
  defp default(nil, value), do: value
  defp default(value, _), do: value

  @doc """
  Sets a specific context key to a specific value

  See `set_context/2` for more information.
  """
  @spec put_context(t() | Ash.Resource.t(), atom, term) :: t()
  def put_context(query, key, value) do
    query = to_query(query)
    set_context(query, %{key => value})
  end

  @doc """
  Set the result of the action. This will prevent running the underlying datalayer behavior
  """
  @spec set_result(t(), term) :: t()
  def set_result(changeset, result) do
    set_context(changeset, %{private: %{action_result: result}})
  end

  @doc """
  Removes a result set previously with `set_result/2`
  """
  @spec clear_result(t()) :: t()
  def clear_result(changeset) do
    %{
      changeset
      | context: Map.update(changeset.context, :private, %{}, &Map.delete(&1, :action_result))
    }
  end

  @doc """
  Merge a map of values into the query context
  """
  @spec set_context(t() | Ash.Resource.t(), map | nil) :: t()
  def set_context(query, nil), do: to_query(query)

  def set_context(query, map) do
    query = to_query(query)

    %{query | context: Ash.Helpers.deep_merge_maps(query.context, map)}
  end

  @doc "Gets the value of an argument provided to the query"
  @spec get_argument(t, atom) :: term
  def get_argument(query, argument) when is_atom(argument) do
    Map.get(query.arguments, argument) || Map.get(query.arguments, to_string(argument))
  end

  @doc "fetches the value of an argument provided to the query or `:error`"
  @spec fetch_argument(t, atom) :: {:ok, term} | :error
  def fetch_argument(query, argument) when is_atom(argument) do
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
  Add an argument to the query, which can be used in filter templates on actions
  """
  def set_argument(query, argument, value) do
    maybe_already_validated_error!(query)
    query = to_query(query)

    if query.action do
      argument =
        Enum.find(
          query.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      with {:arg, argument} when not is_nil(argument) <- {:arg, argument},
           {:ok, casted} <-
             Ash.Type.Helpers.cast_input(argument.type, value, argument.constraints, query),
           {:constrained, {:ok, casted}, argument} when not is_nil(casted) <-
             {:constrained,
              Ash.Type.apply_constraints(argument.type, casted, argument.constraints),
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
        add_error(query, InvalidArgument.exception(Keyword.put(opts, :value, value)))
      end)
    end)
  end

  @doc """
  Remove an argument from the query
  """
  def delete_argument(query, argument_or_arguments) do
    query = to_query(query)

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
    query = to_query(query)
    %{query | arguments: Map.merge(query.arguments, map)}
  end

  defp argument_default(value) when is_function(value, 0), do: value.()
  defp argument_default(value), do: value

  def struct?(%_{}), do: true
  def struct?(_), do: false

  @spec set_tenant(t() | Ash.Resource.t(), String.t()) :: t()
  def set_tenant(query, tenant) do
    query = to_query(query)
    %{query | tenant: tenant}
  end

  @doc "Removes a field from the list of fields to load"
  @spec unload(t(), list(atom)) :: t()
  def unload(query, fields) do
    query = to_query(query)

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
      doc: "A filter keyword, expression or %Ash.Filter{}"
    ],
    sort: [
      type: :any,
      doc: "A sort list or keyword"
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

  #{Spark.OptionsHelpers.docs(@build_opts)}
  """
  @spec build(Ash.Resource.t(), Ash.Api.t() | nil, Keyword.t()) :: t()
  def build(resource, api \\ nil, keyword) do
    Enum.reduce(keyword, new(resource, api), fn
      {:filter, value}, query ->
        do_filter(query, value)

      {:sort, value}, query ->
        sort(query, value)

      {:limit, value}, query ->
        limit(query, value)

      {:offset, value}, query ->
        offset(query, value)

      {:load, value}, query ->
        load(query, value)

      {:distinct, value}, query ->
        distinct(query, value)

      {:lock, lock_type}, query ->
        lock(query, lock_type)

      {:aggregate, {name, type, relationship}}, query ->
        aggregate(query, name, type, relationship)

      {:aggregate, {name, type, relationship, agg_query}}, query ->
        aggregate(query, name, type, relationship, agg_query)

      {:calculate, {name, module_and_opts, type}}, query ->
        calculate(query, name, module_and_opts, type)

      {:calculate, {name, module_and_opts, type, context}}, query ->
        calculate(query, name, module_and_opts, type, context)

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

  @doc "Set the query's api, and any loaded query's api"
  def set_api(query, api) do
    query = to_query(query)
    %{query | api: api, load: set_load_api(query.load, api)}
  end

  @doc """
  Adds an aggregation to the query.

  Aggregations are made available on the `aggregates` field of the records returned

  The filter option accepts either a filter or a keyword list of options to supply to build a limiting query for that aggregate.
  See the DSL docs for each aggregate type in `Ash.Resource.Dsl` for more information.

  Options:

    * query: The query over the destination resource to use as a base for aggregation
    * default: The default value to use if the aggregate returns nil
    * filterable?: Wether or not this aggregate may be referenced in filters
    * type: The type of the aggregate
    * constraints: Type constraints for the aggregate's type
    * implementation: An implementation used when the aggregate kind is custom
    * read_action: The read action to use on the destination resource
    * authorize?: Wether or not to authorize access to this aggregate
  """
  def aggregate(query, name, kind, relationship) do
    aggregate(query, name, kind, relationship, [])
  end

  def aggregate(query, name, kind, relationship, opts) when is_list(opts) do
    aggregate(
      query,
      name,
      kind,
      relationship,
      opts[:query],
      opts[:default],
      Keyword.get(opts, :filterable?, true),
      opts[:type],
      Keyword.get(opts, :constraints, []),
      opts[:implementation],
      opts[:uniq?],
      opts[:read_action],
      Keyword.get(opts, :authorize?, true)
    )
  end

  @doc """
  Adds an aggregation to the query.

  Aggregations are made available on the `aggregates` field of the records returned

  The filter option accepts either a filter or a keyword list of options to supply to build a limiting query for that aggregate.
  See the DSL docs for each aggregate type in `Ash.Resource.Dsl` for more information.
  """
  @spec aggregate(
          t() | Ash.Resource.t(),
          atom(),
          Ash.Query.Aggregate.kind(),
          atom | list(atom),
          Ash.Query.t() | Keyword.t() | nil
        ) :: t()
  def aggregate(
        query,
        name,
        kind,
        relationship,
        agg_query,
        default \\ nil,
        filterable? \\ true,
        type \\ nil,
        constraints \\ [],
        implementation \\ nil,
        uniq? \\ false,
        read_action \\ nil,
        authorize? \\ true
      ) do
    {field, agg_query} =
      case agg_query do
        %Ash.Query{} = query ->
          {nil, query}

        agg_query ->
          Keyword.pop(agg_query || [], :field)
      end

    query = to_query(query)
    relationship = List.wrap(relationship)

    if Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, kind}) do
      related = Ash.Resource.Info.related(query.resource, relationship)

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
            |> build(options)

          %Ash.Query{} = query ->
            query
        end

      case Aggregate.new(
             query.resource,
             name,
             kind,
             path: relationship,
             query: agg_query,
             field: field,
             default: default,
             filterable?: filterable?,
             type: type,
             constraints: constraints,
             implementation: implementation,
             uniq?: uniq?,
             read_action: read_action,
             authorize?: authorize?
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

  Calculations are made available on the `calculations` field of the records returned

  The `module_and_opts` argument accepts either a `module` or a `{module, opts}`. For more information
  on what that module should look like, see `Ash.Calculation`.
  """
  def calculate(query, name, module_and_opts, type, context \\ %{}, constraints \\ []) do
    query = to_query(query)

    {module, opts} =
      case module_and_opts do
        {module, opts} -> {module, opts}
        module -> {module, []}
      end

    case Calculation.new(
           name,
           module,
           opts,
           {type, constraints},
           Map.put(context, :context, query.context)
         ) do
      {:ok, calculation} ->
        fields_to_select =
          query
          |> module.select(opts, calculation.context)
          |> List.wrap()
          |> Enum.concat(calculation.select)
          |> Enum.filter(&Ash.Resource.Info.attribute(query.resource, &1))

        loads =
          module.load(
            query,
            opts,
            Map.put(calculation.context, :context, query.context)
          )
          |> Ash.Actions.Helpers.validate_calculation_load!(module)
          |> Enum.concat(List.wrap(calculation.required_loads))
          |> reify_calculations(query)

        calculation = %{calculation | select: fields_to_select, required_loads: loads}
        %{query | calculations: Map.put(query.calculations, name, calculation)}

      {:error, error} ->
        add_error(query, :calculations, error)
    end
  end

  @doc """
  Adds a resource calculation to the query as a custom calculation with the provided name.

  Example:

      Ash.Query.load_calculation_as(query, :calculation, :some_name, args: %{}, load_through: [:foo])
  """
  def load_calculation_as(query, calc_name, as_name, opts_or_args \\ %{}, opts \\ []) do
    query = to_query(query)

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

  @doc "Limit the results returned from the query"
  @spec limit(t() | Ash.Resource.t(), nil | integer()) :: t()
  def limit(query, nil), do: to_query(query)

  def limit(query, limit) when is_integer(limit) do
    query = to_query(query)

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

  @doc "Skip the first n records"
  @spec offset(t() | Ash.Resource.t(), nil | integer()) :: t()
  def offset(query, nil), do: to_query(query)

  def offset(query, offset) when is_integer(offset) do
    query = to_query(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :offset) do
      query
      |> Map.put(:offset, max(0, offset))
    else
      add_error(query, :offset, "Data layer does not support offset")
    end
  end

  def offset(query, offset) do
    query
    |> to_query()
    |> add_error(:offset, InvalidOffset.exception(offset: offset))
  end

  defp load_relationship(query, statement) do
    query = to_query(query)

    with sanitized_statement <- List.wrap(sanitize_loads(statement)),
         :ok <-
           validate_load(query, sanitized_statement),
         new_loads <- merge_load(query.load, sanitized_statement) do
      %{query | load: new_loads}
    else
      {:error, errors} ->
        Enum.reduce(errors, query, &add_error(&2, &1))
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
    query = to_query(query)

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
                    resource: relationship.destination,
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
  def do_filter(query, %Ash.Filter{} = filter) do
    query = to_query(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :filter) do
      new_filter =
        case query.filter do
          nil ->
            Ash.Filter.parse(query.resource, filter, query.aggregates, query.calculations)

          existing_filter ->
            Ash.Filter.add_to_filter(
              existing_filter,
              filter,
              :and,
              query.aggregates,
              query.calculations
            )
        end

      case new_filter do
        {:ok, filter} ->
          %{query | filter: filter}

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  def do_filter(query, nil), do: to_query(query)
  def do_filter(query, []), do: to_query(query)

  def do_filter(query, statement) do
    query = to_query(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :filter) do
      filter =
        if query.filter do
          Ash.Filter.add_to_filter(
            query.filter,
            statement,
            :and,
            query.aggregates,
            query.calculations
          )
        else
          Ash.Filter.parse(
            query.resource,
            statement,
            query.aggregates,
            query.calculations
          )
        end

      case filter do
        {:ok, filter} ->
          Map.put(query, :filter, filter)

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  @doc """
  Lock the query results.

  This must be run while in a transaction, and is not supported by all data layers.
  """
  @spec lock(t() | Ash.Resource.t(), Ash.DataLayer.lock_type()) :: t()
  def lock(query, nil), do: %{query | lock: nil}

  def lock(query, lock_type) do
    query = to_query(query)

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

  Calculations are supported if they are defined with expressions, which can be done one of two ways.

  1. with the shorthand `calculate :calc, :type, expr(a + b)`
  2. By defining `expression/2` in a custom calculation module

  See the guide on calculations for more.

  Takes a list of fields to sort on, or a keyword list/mixed keyword list of fields and sort directions.
  The default sort direction is `:asc`.

  Examples:

  ```
  Ash.Query.sort(query, [:foo, :bar])

  Ash.Query.sort(query, [:foo, bar: :desc])

  Ash.Query.sort(query, [foo: :desc, bar: :asc])
  ```

  ## Options

  - `prepend?` - set to `true` to put your sort at the front of the list of a sort is already specified
  """
  @spec sort(t() | Ash.Resource.t(), Ash.Sort.t(), opts :: Keyword.t()) :: t()
  def sort(query, sorts, opts \\ []) do
    query = to_query(query)

    if sorts == [] || sorts == nil do
      query
    else
      if Ash.DataLayer.data_layer_can?(query.resource, :sort) do
        if opts[:prepend?] && query.sort != [] do
          query_sort = query.sort

          query
          |> Map.put(:sort, [])
          |> sort(sorts)
          |> sort(query_sort)
        else
          sorts
          |> List.wrap()
          |> Enum.reduce(query, fn
            {sort, direction}, query ->
              %{query | sort: query.sort ++ [{sort, direction}]}

            sort, query ->
              %{query | sort: query.sort ++ [{sort, :asc}]}
          end)
          |> validate_sort()
        end
      else
        add_error(query, :sort, "Data layer does not support sorting")
      end
    end
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
    query = to_query(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :distinct) do
      %{query | distinct: List.wrap(query.distinct) ++ List.wrap(distincts)}
    else
      add_error(query, :distinct, "Data layer does not support distincting")
    end
  end

  @spec unset(Ash.Resource.t() | t(), atom | [atom]) :: t()
  def unset(query, keys) when is_list(keys) do
    query = to_query(query)

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
    |> to_query()
    |> struct([{key, Map.get(new, key)}])
  end

  defp do_unset(query, key, _new) when key in [:api, :resource] do
    query
  end

  defp do_unset(query, :load, new) do
    query = unset(query, [:calculations, :aggregates])

    struct(query, [{:load, Map.get(new, :load)}])
  end

  defp do_unset(query, key, new) do
    struct(query, [{key, Map.get(new, key)}])
  end

  @doc "Return the underlying data layer query for an ash query"
  def data_layer_query(ash_query, opts \\ [])

  def data_layer_query(%{errors: errors}, _opts) when errors not in [[], nil] do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def data_layer_query(%{resource: resource, api: api} = ash_query, opts) do
    query = opts[:initial_query] || Ash.DataLayer.resource_to_query(resource, api)

    filter_aggregates =
      if ash_query.filter do
        Ash.Filter.used_aggregates(ash_query.filter)
      else
        []
      end

    sort_aggregates =
      Enum.flat_map(ash_query.sort, fn {field, _} ->
        case Map.fetch(ash_query.aggregates, field) do
          :error ->
            []

          {:ok, agg} ->
            [agg]
        end
      end)

    aggregates = Enum.uniq_by(filter_aggregates ++ sort_aggregates, & &1.name)

    with {:ok, query} <-
           Ash.DataLayer.set_context(
             resource,
             query,
             Map.put(ash_query.context, :action, ash_query.action)
           ),
         {:ok, query} <-
           add_tenant(query, ash_query),
         {:ok, query} <- Ash.DataLayer.select(query, ash_query.select, ash_query.resource),
         {:ok, query} <-
           add_aggregates(query, ash_query, aggregates),
         {:ok, query} <-
           Ash.DataLayer.sort(query, ash_query.sort, resource),
         {:ok, query} <- maybe_filter(query, ash_query, opts),
         {:ok, query} <- Ash.DataLayer.distinct(query, ash_query.distinct, resource),
         {:ok, query} <-
           Ash.DataLayer.limit(query, ash_query.limit, resource),
         {:ok, query} <-
           Ash.DataLayer.offset(query, ash_query.offset, resource),
         {:ok, query} <- Ash.DataLayer.lock(query, ash_query.lock, resource) do
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

  defp add_tenant(query, ash_query) do
    with :context <- Ash.Resource.Info.multitenancy_strategy(ash_query.resource),
         tenant when not is_nil(tenant) <- ash_query.tenant,
         {:ok, query} <- Ash.DataLayer.set_tenant(ash_query.resource, query, tenant) do
      {:ok, query}
    else
      {:error, error} -> {:error, error}
      _ -> {:ok, query}
    end
  end

  defp add_aggregates(query, ash_query, aggregates) do
    resource = ash_query.resource

    used =
      ash_query
      |> Ash.Query.Aggregate.aggregates_from_filter()
      |> Enum.map(&elem(&1, 1))

    aggregates =
      aggregates
      |> Enum.concat(used)
      |> Enum.map(&add_tenant_to_aggregate_query(&1, ash_query))

    Ash.DataLayer.add_aggregates(query, aggregates, resource)
  end

  defp add_tenant_to_aggregate_query(aggregate, %{tenant: nil}), do: aggregate

  defp add_tenant_to_aggregate_query(%{query: nil} = aggregate, ash_query) do
    aggregate_with_query = %{aggregate | query: new(aggregate.resource)}
    add_tenant_to_aggregate_query(aggregate_with_query, ash_query)
  end

  defp add_tenant_to_aggregate_query(aggregate, ash_query) do
    case Ash.Resource.Info.multitenancy_strategy(aggregate.resource) do
      nil ->
        aggregate

      :attribute ->
        attribute = Ash.Resource.Info.multitenancy_attribute(aggregate.resource)
        {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(ash_query.resource)
        attribute_value = apply(m, f, [ash_query.tenant | a])
        %{aggregate | query: filter(aggregate.query, ^[{attribute, attribute_value}])}

      :context ->
        %{aggregate | query: set_tenant(aggregate.query, ash_query.tenant)}
    end
  end

  defp validate_sort(%{resource: resource, sort: sort} = query) do
    case Sort.process(resource, sort, query.aggregates, query.context) do
      {:ok, new_sort} -> %{query | sort: new_sort}
      {:error, error} -> add_error(query, :sort, error)
    end
  end

  def add_error(query, keys \\ [], message) do
    keys = List.wrap(keys)
    query = to_query(query)

    message =
      if is_binary(message) do
        string_path =
          case keys do
            [key] -> to_string(key)
            keys -> Enum.join(keys, ".")
          end

        "#{string_path}: #{message}"
      else
        message
      end

    message
    |> Ash.Error.to_ash_error()
    |> case do
      errors when is_list(errors) ->
        errors =
          Enum.map(errors, fn error ->
            Map.update(error, :path, keys, &(keys ++ List.wrap(&1)))
          end)

        %{query | errors: query.errors ++ errors, valid?: false}

      error ->
        error = Map.update(error, :path, keys, &(keys ++ List.wrap(&1)))
        %{query | errors: [error | query.errors], valid?: false}
    end
  end

  defp validate_matching_query_and_continue(value, resource, key, path, relationship) do
    %{destination: relationship_resource} = relationship

    case value do
      %__MODULE__{resource: query_resource} = destination_query
      when query_resource != relationship_resource ->
        [
          InvalidQuery.exception(
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
            InvalidQuery.exception(
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

  defp set_load_api(nil, _), do: nil
  defp set_load_api([], _), do: []

  defp set_load_api(%__MODULE__{} = query, api) do
    set_api(query, api)
  end

  defp set_load_api(loads, api) do
    Enum.map(loads, fn {key, further} ->
      {key, set_load_api(further, api)}
    end)
  end

  @doc """
  Takes a resource or a query and returns a query.
  """
  @spec to_query(t() | Ash.Resource.t()) :: t()
  def to_query(%__MODULE__{} = query), do: query

  def to_query(resource) do
    resource
    |> new()
    |> Ash.DataLayer.transform_query()
  end

  defp merge_load([], right), do: sanitize_loads(right)
  defp merge_load(left, []), do: sanitize_loads(left)

  defp merge_load(
         %__MODULE__{load: left_loads, calculations: left_calculations, tenant: left_tenant},
         %__MODULE__{load: right_loads, calculations: right_calculations} = query
       ) do
    %{
      query
      | load: merge_load(left_loads, right_loads),
        calculations: Map.merge(left_calculations, right_calculations)
    }
    |> set_tenant(query.tenant || left_tenant)
  end

  defp merge_load(%__MODULE__{} = query, right) when is_list(right) do
    load_relationship(query, right)
  end

  defp merge_load(left, %Ash.Query{} = query) when is_list(left) do
    load_relationship(query, left)
  end

  defp merge_load(left, right) when is_atom(left), do: merge_load([{left, []}], right)
  defp merge_load(left, right) when is_atom(right), do: merge_load(left, [{right, []}])

  defp merge_load(left, right) when is_list(left) and is_list(right) do
    right
    |> sanitize_loads()
    |> Enum.reduce(sanitize_loads(left), fn {rel, rest}, acc ->
      Keyword.update(acc, rel, rest, &merge_load(&1, rest))
    end)
  end

  defp sanitize_loads(load) when is_atom(load), do: {load, []}

  defp sanitize_loads(%Ash.Query{} = query) do
    Map.update!(query, :load, &sanitize_loads/1)
  end

  defp sanitize_loads(loads) do
    loads
    |> List.wrap()
    |> Enum.map(fn
      {key, value} ->
        {key, sanitize_loads(value)}

      load_part ->
        cond do
          is_atom(load_part) -> {load_part, []}
          is_list(load_part) -> sanitize_loads(load_part)
          true -> load_part
        end
    end)
  end
end
