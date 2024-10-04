defmodule Ash.Query do
  @moduledoc """
  A data structure for reading data from a resource.

  Ash queries are used for read actions and loads, and ultimately
  map to queries to a resource's data layer.

  Queries are run by calling `Ash.read`.

  Examples:

  ```elixir
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
    around_transaction: [],
    invalid_keys: MapSet.new(),
    load_through: %{},
    action_failed?: false,
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

  @type t :: %__MODULE__{
          __validated_for_action__: atom | nil,
          action: Ash.Resource.Actions.Read.t() | nil,
          domain: module | nil,
          distinct: [atom],
          filter: Ash.Filter.t() | nil,
          resource: module,
          tenant: term(),
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
          calculations: %{optional(atom) => :wat},
          context: map,
          errors: [Ash.Error.t()],
          limit: nil | non_neg_integer(),
          load: keyword(keyword),
          offset: non_neg_integer(),
          page: keyword() | nil,
          params: %{optional(atom | binary) => any},
          phase: :preparing | :before_action | :after_action | :executing,
          select: nil | [atom],
          sort: [atom | {atom, :asc | :desc}],
          valid?: boolean
        }

  @type around_result ::
          {:ok, list(Ash.Resource.record())}
          | {:error, Ash.Error.t()}
  @type around_callback :: (t() -> around_result)
  @type around_action_fun :: (t, around_callback -> around_result)

  @type around_transaction_fun :: (t -> {:ok, Ash.Resource.record()} | {:error, any})

  alias Ash.Actions.Sort

  alias Ash.Error.Invalid.TimeoutNotSupported

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidArgument,
    InvalidCalculationArgument,
    InvalidLimit,
    InvalidOffset,
    InvalidPage,
    NoReadAction,
    ReadActionRequiresActor,
    Required
  }

  alias Ash.Error.Load.{InvalidQuery, NoSuchRelationship}
  alias Ash.Query.{Aggregate, Calculation}

  require Ash.Tracer
  import Ash.Expr, only: [expr: 1, expr?: 1]

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

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", inspect(query.resource)),
          or_empty(concat("tenant: ", to_doc(query.to_tenant, opts)), tenant?),
          arguments(query, opts),
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

      if !is_atom(query) && query.__validated_for_action__ &&
           !query.context[:private][:in_before_action?] do
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
  Attach a sort statement to the query labelled as user input.

  Sorts added as user input (or filters constructed with `Ash.Filter.parse_input`)
  will honor any field policies on resources by replacing any references to the field
  with `nil` in cases where the actor should not be able to see the given field.
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
              |> validate_sort()

            {:error, error} ->
              Ash.Query.add_error(query, :sort, error)
          end
        end
      else
        add_error(query, :sort, "Data layer does not support sorting")
      end
    end
    |> sequence_expr_sorts()
  end

  # sobelow_skip ["DOS.BinToAtom", "DOS.StringToAtom"]
  defp sequence_expr_sorts(%{sort: sort} = query) when is_list(sort) and sort != [] do
    %{
      query
      | sort:
          query.sort
          |> Enum.with_index()
          |> Enum.map(fn
            {{%Ash.Query.Calculation{name: :__expr_sort__} = field, direction}, index} ->
              {%{field | name: String.to_atom("__expr_sort__#{index}"), load: nil}, direction}

            {other, _} ->
              other
          end)
    }
  end

  defp sequence_expr_sorts(query), do: query

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
  @spec new(Ash.Resource.t() | Ash.Query.t(), opts :: Keyword.t()) :: Ash.Query.t()
  def new(resource, opts \\ [])
  def new(%__MODULE__{} = query, _opts), do: query

  def new(resource, opts) when is_atom(resource) do
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
    ]
  ]

  @doc false
  def for_read_opts, do: @for_read_opts

  @doc """
  Creates a query for a given read action and prepares it.

  Multitenancy is *not* validated until an action is called. This allows you to avoid specifying a tenant until just before calling
  the domain action.

  ### Arguments
  Provide a map or keyword list of arguments for the read action

  ### Opts

  #{Spark.Options.docs(@for_read_opts)}

  """
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
    skip_unknown_inputs = opts[:skip_unknown_inputs] || []

    Enum.reduce(args, query, fn {name, value}, query ->
      cond do
        has_argument?(action, name) ->
          set_argument(query, name, value)

        :* in List.wrap(opts[:skip_unknown_inputs]) ->
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
    query.resource
    |> Ash.Resource.Info.preparations()
    |> Enum.concat(action.preparations || [])
    |> Enum.reduce_while(query, fn %{preparation: {module, opts}}, query ->
      Ash.Tracer.span :preparation, fn -> "prepare: #{inspect(module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :preparation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            preparation: inspect(module)
          }
        end do
          Ash.Tracer.set_metadata(opts[:tracer], :preparation, metadata)

          case module.init(opts) do
            {:ok, opts} ->
              opts =
                Ash.Expr.fill_template(
                  opts,
                  actor,
                  query.arguments,
                  query.context
                )

              case module.prepare(query, opts, %Ash.Resource.Preparation.Context{
                     tenant: query.tenant,
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

  @doc """
  Adds an around_transaction hook to the query.

  Your function will get the query, and a callback that must be called with a query (that may be modified).
  The callback will return `{:ok, results}` or `{:error, error}`. You can modify these values, but the return value
  must be one of those types.

  The around_transaction calls happen first, and then (after they each resolve their callbacks) the `before_action`
  hooks are called, followed by the `after_action` hooks being run. Then, the code that appeared *after* the callbacks were called is then run.

  Warning: using this without understanding how it works can cause big problems.
  You *must* call the callback function that is provided to your hook, and the return value must
  contain the same structure that was given to you, i.e `{:ok, result_of_action}`.
  """

  @spec around_transaction(t(), around_transaction_fun()) :: t()
  def around_transaction(query, func) do
    query = new(query)
    %{query | around_transaction: query.around_transaction ++ [func]}
  end

  @doc """
  Adds a before_action hook to the query.

  Provide the option `prepend?: true` to place the hook before all
  other hooks instead of after.
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

  @spec after_action(
          t(),
          (t(), [Ash.Resource.record()] ->
             {:ok, [Ash.Resource.record()]}
             | {:ok, [Ash.Resource.record()], list(Ash.Notifier.Notification.t())}
             | {:error, term})
        ) :: t()
  def after_action(query, func) do
    query = new(query)
    %{query | after_action: [func | query.after_action]}
  end

  defp add_action_filters(query, %{filter: nil}, _actor), do: query

  defp add_action_filters(query, action, actor) do
    if Ash.Expr.template_references_actor?(action.filter) and is_nil(actor) do
      Ash.Query.add_error(query, ReadActionRequiresActor.exception([]))
    else
      built_filter =
        Ash.Expr.fill_template(
          action.filter,
          actor,
          query.arguments,
          query.context
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
  """
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
  Ensure the the specified attributes are `nil` in the query results.
  """
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

          attribute && (attribute.primary_key? || !attribute.public?)
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

  By default, loading attributes has no effects, as all attributes are returned.

  ```elixir
  # Loading nested relationships
  Ash.Query.load(query, [comments: [:author, :ratings]])

  # Loading relationships with a query
  Ash.Query.load(query, [comments: [author: author_query]])
  ```

  By passing the `strict?: true` option, only specified attributes will be loaded when passing
  a list of fields to fetch on a relationship, which allows for more optimized data-fetching.

  The select statement of any queries inside the load statement will not be affected.

  Example:
  ```elixir
  Ash.load(category, [:name, posts: [:title, :published_at]], strict?: true)
  ```

  Here, the only fields that will be loaded on the `posts` relationship are `title` and
  `published_at`, in addition to any other fields that are required to be loaded, like the
  primary and relevant foreign keys.
  This entails that when using `strict?: true` and loading nested relationships, you will also
  always have to specify all the attributes you want to load alongside the nested relationships.

  Example:
  ```elixir
  Ash.load(post, [:title, :published_at, :other_needed_attribute, category: [:name]], strict?: true)
  ```

  If no fields are specified on a relationship when using `strict?: true`, all attributes will be
  loaded by default.

  Example:
  ```elixir
  Ash.load(category, [:name, :posts], strict?: true)
  ```
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

  def load(query, %Ash.Query{} = new, _opts) do
    query |> new() |> merge_load(new)
  end

  def load(query, fields, opts) when not is_list(fields) do
    load(query, List.wrap(fields), opts)
  end

  def load(query, fields, opts) do
    strict? = Keyword.get(opts, :strict?, false)

    query =
      if strict? do
        query
        |> new()
        |> select([])
      else
        new(query)
      end

    Enum.reduce(fields, query, fn
      %Ash.Query{} = new, query ->
        merge_load(query, new)

      [], query ->
        query

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
            nested_query = load(rel.destination, rest, opts)

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

    resource_calculation_load =
      if resource_calculation do
        List.wrap(resource_calculation.load)
      else
        []
      end

    loads =
      module.load(
        query,
        opts,
        Map.put(calculation.context, :context, query.context)
      )
      |> Ash.Actions.Helpers.validate_calculation_load!(module)
      |> Enum.concat(resource_calculation_load)

    %{calculation | required_loads: loads}
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
           Ash.Query.Calculation.from_resource_calculation!(query.resource, resource_calculation,
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

  defp do_load(query, field) when is_list(field) do
    Enum.reduce(field, query, &do_load(&2, &1))
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

      match?(%{__struct__: Ash.Query.Aggregate}, field) ->
        Map.update!(
          query,
          :aggregates,
          &Map.put(
            &1,
            field.name,
            field
          )
        )

      Ash.Resource.Info.attribute(query.resource, field) ->
        ensure_selected(query, field)

      Ash.Resource.Info.relationship(query.resource, field) ->
        load_relationship(query, field)

      aggregate = Ash.Resource.Info.aggregate(query.resource, field) ->
        with {:can?, true} <-
               {:can?,
                Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, aggregate.kind})},
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
                 join_filters: Map.new(aggregate.join_filters, &{&1.relationship_path, &1.filter})
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

    has_one_expr? = Enum.any?(args, fn {_, value} -> expr?(value) end)

    args
    |> Enum.reduce_while({:ok, %{}}, fn {key, value}, {:ok, arg_values} ->
      argument =
        if is_binary(key) do
          Enum.find(calculation.arguments, fn arg -> to_string(arg.name) == key end)
        else
          Enum.find(calculation.arguments, fn arg -> arg.name == key end)
        end

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

        expr?(value) && argument.allow_expr? && allow_expr? ->
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
      if Map.has_key?(inputs, argument.name) do
        if is_nil(inputs[argument.name]) && !argument.allow_nil? do
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
      else
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
  Sets a specific context key to a specific value

  See `set_context/2` for more information.
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
        add_error(query, InvalidArgument.exception(Keyword.put(opts, :value, value)))
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

  def struct?(%_{}), do: true
  def struct?(_), do: false

  @spec set_tenant(t() | Ash.Resource.t(), Ash.ToTenant.t()) :: t()
  def set_tenant(query, tenant) do
    query = new(query)
    %{query | tenant: tenant, to_tenant: Ash.ToTenant.to_tenant(tenant, query.resource)}
  end

  @doc """
  Sets the pagination options of the query.

  Pass `nil` to disable pagination.

  ### Limit/offset pagination
  #{Spark.Options.docs(Ash.Page.Offset.page_opts())}

  ### Keyset pagination
  #{Spark.Options.docs(Ash.Page.Keyset.page_opts())}
  """
  @spec page(t() | Ash.Resource.t(), Keyword.t()) :: t()
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

      {:distinct_sort, value}, query ->
        distinct_sort(query, value)

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

  Aggregations are made available on the `aggregates` field of the records returned

  The filter option accepts either a filter or a keyword list of options to supply to build a limiting query for that aggregate.
  See the DSL docs for each aggregate type in the [Resource DSL docs](dsl-ash-resource.html#aggregates) for more information.

  Options:

    * query: The query over the destination resource to use as a base for aggregation
    * default: The default value to use if the aggregate returns nil
    * filterable?: Whether or not this aggregate may be referenced in filters
    * type: The type of the aggregate
    * constraints: Type constraints for the aggregate's type
    * implementation: An implementation used when the aggregate kind is custom
    * read_action: The read action to use on the destination resource
    * authorize?: Whether or not to authorize access to this aggregate
    * join_filters: A map of relationship paths to filter expressions. See the aggregates guide for more.
  """
  def aggregate(query, name, kind, relationship) do
    aggregate(query, name, kind, relationship, [])
  end

  def aggregate(query, name, kind, relationship, opts) when is_list(opts) do
    agg_query = opts[:query]
    default = opts[:default]
    filterable? = Keyword.get(opts, :filterable?, true)
    sortable? = Keyword.get(opts, :filterable?, true)
    type = opts[:type]
    constraints = opts[:constraints] || []
    implementation = opts[:implementation]
    include_nil? = Keyword.get(opts, :include_nil?, true)
    uniq? = opts[:uniq?]
    read_action = opts[:read_action]
    authorize? = Keyword.get(opts, :authorize?, true)
    join_filters = Keyword.get(opts, :join_filters, %{})
    sensitive? = Keyword.get(opts, :sensitive?, false)

    {field, agg_query} =
      case agg_query do
        %Ash.Query{} = query ->
          {nil, query}

        agg_query ->
          Keyword.pop(agg_query || [], :field)
      end

    query = new(query)
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
            |> Ash.Query.Aggregate.build_query(query.resource, options)

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
  on what that module should look like, see `Ash.Resource.Calculation`.
  """
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
          {module, opts}

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

  @doc "Limit the results returned from the query"
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

  @doc "Skip the first n records"
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

  defp load_relationship(query, statement) do
    query = new(query)

    with sanitized_statement <- List.wrap(sanitize_loads(statement)),
         :ok <- validate_load(query, sanitized_statement),
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
    query = new(query)

    if sorts == [] || sorts == nil do
      query
    else
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
          |> validate_sort()
        end
      else
        add_error(query, :sort, "Data layer does not support sorting")
      end
    end
    |> sequence_expr_sorts()
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
          |> validate_sort()
        end
      else
        add_error(query, :distinct_sort, "Data layer does not support distinct sorting")
      end
    end
  end

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
    query = opts[:initial_query] || Ash.DataLayer.resource_to_query(resource, domain)

    context =
      ash_query.context
      |> Map.put(:action, ash_query.action)
      |> Map.put_new(:private, %{})
      |> put_in([:private, :tenant], ash_query.tenant)

    with {:ok, query} <-
           Ash.DataLayer.set_context(
             resource,
             query,
             context
           ),
         {:ok, query} <- add_tenant(query, ash_query),
         {:ok, query} <- Ash.DataLayer.select(query, ash_query.select, ash_query.resource),
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

  defp validate_sort(%{resource: resource, sort: sort} = query) do
    case Sort.process(resource, sort, query.aggregates, query.context) do
      {:ok, new_sort} -> %{query | sort: new_sort}
      {:error, error} -> add_error(query, :sort, error)
    end
  end

  def add_error(query, keys \\ [], message) do
    keys = List.wrap(keys)
    query = new(query)

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

  defp merge_load([], %Ash.Query{} = right), do: right
  defp merge_load(%Ash.Query{} = left, []), do: left
  defp merge_load([], right), do: sanitize_loads(right)
  defp merge_load(left, []), do: sanitize_loads(left)

  defp merge_load(
         %__MODULE__{
           resource: resource,
           load: left_loads,
           calculations: left_calculations,
           aggregates: left_aggregates,
           tenant: left_tenant,
           select: left_select
         },
         %__MODULE__{
           load: right_loads,
           aggregates: right_aggregates,
           calculations: right_calculations,
           select: right_select
         } =
           query
       ) do
    select =
      if is_nil(left_select) or is_nil(right_select) do
        all_attribute_names(resource)
      else
        Enum.uniq(left_select ++ right_select)
      end

    %{
      query
      | load: merge_load(left_loads, right_loads),
        calculations: Map.merge(left_calculations, right_calculations),
        aggregates: Map.merge(left_aggregates, right_aggregates),
        select: select
    }
    |> set_tenant(query.tenant || left_tenant)
  end

  defp merge_load(%__MODULE__{} = query, right) when is_list(right) do
    load_relationship(query, right)
  end

  defp merge_load(left, %__MODULE__{} = query) when is_list(left) do
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

  defp all_attribute_names(resource) do
    resource |> Ash.Resource.Info.attributes() |> Enum.map(& &1.name)
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
