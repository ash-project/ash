defmodule Ash.Query do
  @moduledoc """
  Utilties around constructing/manipulating ash queries.

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
  |> Ash.Query.aggregate(:published_post_count, :posts, filter: [published: true])
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
    :api,
    :resource,
    :filter,
    :tenant,
    :action,
    :distinct,
    :__validated_for_action__,
    params: %{},
    arguments: %{},
    aggregates: %{},
    load: [],
    calculations: %{},
    context: %{},
    select: nil,
    sort: [],
    limit: nil,
    offset: 0,
    errors: [],
    action_failed?: false,
    before_action: [],
    after_action: [],
    valid?: true
  ]

  @type t :: %__MODULE__{}

  alias Ash.Actions.Sort

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidArgument,
    InvalidLimit,
    InvalidOffset,
    NoReadAction,
    Required
  }

  alias Ash.Error.Load.{InvalidQuery, NoSuchRelationship}
  alias Ash.Query.{Aggregate, BooleanExpression, Calculation, Not}

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(query, opts) do
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
          or_empty(concat("distinct: ", to_doc(query.distinct, opts)), distinct?)
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end

    defp arguments(query, opts) do
      if query.action do
        action = Ash.Resource.Info.action(query.resource, query.action, :read)

        if is_nil(action) || Enum.empty?(action.arguments) do
          empty()
        else
          arg_string =
            action.arguments
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

  @doc """
  Attach a filter statement to the query.

  The filter is applied as an "and" to any filters currently on the query.
  For more information on writing filters, see: `Ash.Filter`.
  """
  defmacro filter(query, %Ash.Filter{} = filter) do
    quote do
      Ash.Query.do_filter(unquote(query), unquote(filter))
    end
  end

  defmacro filter(query, nil), do: query
  defmacro filter(query, true), do: query

  defmacro filter(query, false) do
    quote do
      Ash.Query.do_filter(unquote(query), false)
    end
  end

  defmacro filter(query, do: body) do
    quote do
      Ash.Query.do_filter(unquote(query), unquote(body))
    end
  end

  defmacro filter(query, expression) do
    if Keyword.keyword?(expression) do
      quote do
        Ash.Query.do_filter(unquote(query), unquote(expression))
      end
    else
      expr = do_expr(expression)

      quote do
        Ash.Query.do_filter(unquote(query), List.wrap(unquote(expr)))
      end
    end
  end

  @doc "Create a new query"
  def new(resource, api \\ nil)
  def new(%__MODULE__{} = query, _), do: query

  def new(resource, api) when is_atom(resource) do
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
          filter =
            resource
            |> Ash.Filter.parse!(filter)
            |> Ash.Filter.embed_predicates()

          do_filter(query, filter)
      end

    case Ash.Resource.Info.default_context(resource) do
      nil ->
        query

      context ->
        Ash.Query.set_context(query, context)
    end
  end

  @for_read_opts [
    actor: [
      type: :any,
      doc:
        "set the actor, which can be used in any `Ash.Resource.Change`s configured on the action. (in the `context` argument)"
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

  #{Ash.OptionsHelpers.docs(@for_read_opts)}

  """
  def for_read(query, action_name, args \\ %{}, opts \\ []) do
    query = to_query(query)
    query = %{query | params: Map.merge(query.params || %{}, Enum.into(args, %{}))}

    action = Ash.Resource.Info.action(query.resource, action_name, :read)

    if action do
      query = Map.put(query, :action, action.name)

      query
      |> Ash.Query.set_tenant(opts[:tenant] || query.tenant)
      |> Map.put(:action, action)
      |> Map.put(:__validated_for_action__, action_name)
      |> cast_params(action, args)
      |> run_preparations(action, opts[:actor])
      |> add_action_filters(action, opts[:actor])
      |> require_arguments(action)
    else
      add_error(query, :action, "No such action #{action_name}")
    end
  end

  defp require_arguments(query, action) do
    query
    |> set_argument_defaults(action)
    |> do_require_arguments(action)
  end

  defp do_require_arguments(query, action) do
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

  defp run_preparations(query, action, actor) do
    Enum.reduce(action.preparations || [], query, fn %{preparation: {module, opts}}, query ->
      module.prepare(query, opts, %{actor: actor})
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
      Ash.Query.add_error(query, "Read action requires actor")
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

  defmacro expr(do: body) do
    quote do
      Ash.Query.expr(unquote(body))
    end
  end

  defmacro expr({var, _, context} = binding) when is_atom(var) and is_atom(context) do
    quote do
      unquote(binding)
    end
  end

  defmacro expr(body) do
    if Keyword.keyword?(body) do
      quote do
        unquote(body)
      end
    else
      expr = do_expr(body)

      quote do
        unquote(expr)
      end
    end
  end

  @operator_symbols Ash.Query.Operator.operator_symbols()

  defp do_expr(expr, escape? \\ true)

  defp do_expr({:^, _, [value]}, _escape?) do
    value
  end

  defp do_expr({{:., _, [_, _]} = left, _, _}, escape?) do
    do_expr(left, escape?)
  end

  defp do_expr({:ref, _, [field, path]}, escape?) do
    ref =
      case do_expr(path, false) do
        %Ash.Query.Ref{attribute: head_attr, relationship_path: head_path} ->
          case do_expr(field) do
            %Ash.Query.Ref{attribute: tail_attribute, relationship_path: tail_relationship_path} ->
              %Ash.Query.Ref{
                relationship_path: head_path ++ [head_attr] ++ tail_relationship_path,
                attribute: tail_attribute
              }

            other ->
              %Ash.Query.Ref{relationship_path: head_path ++ [head_attr], attribute: other}
          end

        other ->
          case do_expr(field, false) do
            %Ash.Query.Ref{attribute: attribute, relationship_path: relationship_path} ->
              %Ash.Query.Ref{
                attribute: attribute,
                relationship_path: List.wrap(other) ++ List.wrap(relationship_path)
              }

            other_field ->
              %Ash.Query.Ref{attribute: other_field, relationship_path: other}
          end
      end

    soft_escape(ref, escape?)
  end

  defp do_expr({:ref, _, [field]}, escape?) do
    ref =
      case do_expr(field, false) do
        %Ash.Query.Ref{} = ref ->
          ref

        other ->
          %Ash.Query.Ref{attribute: other, relationship_path: []}
      end

    soft_escape(ref, escape?)
  end

  defp do_expr({:., _, [left, right]} = ref, escape?) when is_atom(right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{} = ref ->
        soft_escape(ref, escape?)

      :error ->
        raise "Invalid reference! #{Macro.to_string(ref)}"
    end
  end

  defp do_expr({op, _, nil}, escape?) when is_atom(op) do
    soft_escape(%Ash.Query.Ref{relationship_path: [], attribute: op}, escape?)
  end

  defp do_expr({op, _, args}, escape?) when op in [:and, :or] do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(BooleanExpression.optimized_new(op, Enum.at(args, 0), Enum.at(args, 1)), escape?)
  end

  defp do_expr({op, _, [_, _] = args}, escape?)
       when is_atom(op) and op in @operator_symbols do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: true}, escape?)
  end

  defp do_expr({left, _, [{op, _, [right]}]}, escape?)
       when is_atom(op) and op in @operator_symbols and is_atom(left) and left != :not do
    args = Enum.map([{left, [], nil}, right], &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: true}, escape?)
  end

  defp do_expr({:not, _, [expression]}, escape?) do
    expression = do_expr(expression, false)

    soft_escape(Not.new(expression), escape?)
  end

  defp do_expr({op, _, args}, escape?) when is_atom(op) and is_list(args) do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: false}, escape?)
  end

  defp do_expr({left, _, _}, escape?) when is_tuple(left), do: do_expr(left, escape?)

  defp do_expr(other, _), do: other

  defp soft_escape(%_{} = val, _) do
    {:%{}, [], Map.to_list(val)}
  end

  defp soft_escape(other, _), do: other

  defp do_ref({left, _, nil}, right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref({{:., _, [_, _]} = left, _, _}, right) do
    do_ref(left, right)
  end

  defp do_ref({:., _, [left, right]}, far_right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{relationship_path: path, attribute: attribute} = ref ->
        %{ref | relationship_path: path ++ [attribute], attribute: far_right}

      :error ->
        :error
    end
  end

  defp do_ref({left, _, _}, right) when is_atom(left) and is_atom(right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref(_left, _right) do
    :error
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
  """
  def select(query, fields, opts \\ []) do
    query = to_query(query)

    if opts[:replace?] do
      %{query | select: Enum.uniq(List.wrap(fields))}
    else
      %{query | select: Enum.uniq(List.wrap(fields) ++ (query.select || []))}
    end
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
    end
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
  @spec load(t() | Ash.Resource.t(), atom | list(atom) | Keyword.t()) :: t()
  def load(query, fields) when not is_list(fields) do
    load(query, List.wrap(fields))
  end

  def load(query, fields) do
    query = to_query(query)

    Enum.reduce(fields, query, fn
      {field, %__MODULE__{} = nested}, query ->
        load_relationship(query, [{field, nested}])

      {field, rest}, query ->
        cond do
          rel = Ash.Resource.Info.relationship(query.resource, field) ->
            nested_query = load(rel.destination, rest)

            load_relationship(query, [{field, nested_query}])

          resource_calculation = Ash.Resource.Info.calculation(query.resource, field) ->
            {module, opts} = module_and_opts(resource_calculation.calculation)

            with {:ok, args} <- validate_arguments(resource_calculation, rest),
                 {:ok, calculation} <-
                   Calculation.new(
                     resource_calculation.name,
                     module,
                     opts,
                     args
                   ) do
              calculation = %{calculation | load: field}

              fields_to_select =
                resource_calculation.select
                |> Kernel.||([])
                |> Enum.concat(module.select(query, opts) || [])

              query
              |> Map.update!(:calculations, &Map.put(&1, field, calculation))
              |> maybe_select(fields_to_select)
            end

          true ->
            add_error(query, :load, "Invalid load #{inspect(field)}")
        end

      field, query ->
        do_load(query, field)
    end)
  end

  defp module_and_opts({module, opts}), do: {module, opts}
  defp module_and_opts(module), do: {module, []}

  defp do_load(query, field) do
    cond do
      Ash.Resource.Info.attribute(query.resource, field) ->
        query

      Ash.Resource.Info.relationship(query.resource, field) ->
        load_relationship(query, field)

      aggregate = Ash.Resource.Info.aggregate(query.resource, field) ->
        related = Ash.Resource.Info.related(query.resource, aggregate.relationship_path)

        with {:can?, true} <-
               {:can?,
                Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, aggregate.kind})},
             %{valid?: true} = aggregate_query <-
               build(related, filter: aggregate.filter, sort: aggregate.sort),
             {:ok, query_aggregate} <-
               Aggregate.new(
                 query.resource,
                 aggregate.name,
                 aggregate.kind,
                 aggregate.relationship_path,
                 aggregate_query,
                 aggregate.field
               ) do
          query_aggregate = %{query_aggregate | load: field}
          new_aggregates = Map.put(query.aggregates, aggregate.name, query_aggregate)

          %{query | aggregates: new_aggregates}
        else
          %{errors: errors} ->
            add_error(query, :aggregates, Ash.Error.to_ash_error(errors))

          {:error, error} ->
            add_error(query, :aggregates, Ash.Error.to_ash_error(error))

          {:can?, false} ->
            add_error(
              query,
              :aggregate,
              AggregatesNotSupported.exception(resource: query.resource, feature: "using")
            )
        end

      resource_calculation = Ash.Resource.Info.calculation(query.resource, field) ->
        {module, opts} =
          case resource_calculation.calculation do
            {module, opts} -> {module, opts}
            module -> {module, []}
          end

        with {:ok, args} <- validate_arguments(resource_calculation, %{}),
             {:ok, calculation} <-
               Calculation.new(resource_calculation.name, module, opts, args) do
          calculation = %{calculation | load: field}

          fields_to_select =
            resource_calculation.select
            |> Kernel.||([])
            |> Enum.concat(module.select(query, opts) || [])

          query
          |> Map.update!(:calculations, &Map.put(&1, field, calculation))
          |> maybe_select(fields_to_select)
        else
          {:error, error} ->
            add_error(query, :load, error)
        end

      true ->
        add_error(query, :load, "Could not load #{inspect(field)}")
    end
  end

  defp maybe_select(query, field) do
    if query.select do
      Ash.Query.select(query, List.wrap(field))
    else
      to_select =
        query.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      Ash.Query.select(query, to_select)
    end
  end

  defp validate_arguments(calculation, args) do
    Enum.reduce_while(calculation.arguments, {:ok, %{}}, fn argument, {:ok, arg_values} ->
      value = default(Map.get(args, argument.name), argument.default)

      if is_nil(value) do
        if argument.allow_nil? do
          {:cont, {:ok, Map.put(arg_values, argument.name, nil)}}
        else
          {:halt, {:error, "Argument #{argument.name} is required"}}
        end
      else
        with {:ok, casted} <- Ash.Type.cast_input(argument.type, value, argument.constraints),
             {:ok, casted} <-
               Ash.Type.apply_constraints(argument.type, casted, argument.constraints) do
          {:cont, {:ok, Map.put(arg_values, argument.name, casted)}}
        else
          {:error, error} ->
            {:halt, {:error, error}}
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
    query = to_query(query)

    if query.action do
      argument =
        Enum.find(
          query.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      with {:arg, argument} when not is_nil(argument) <- {:arg, argument},
           {:ok, casted} <-
             Ash.Changeset.cast_input(argument.type, value, argument.constraints, query),
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
          add_invalid_errors(query, argument, error)

        {:error, error} ->
          query = %{query | arguments: Map.put(query.arguments, argument.name, value)}
          add_invalid_errors(query, argument, error)

        :error ->
          query = %{query | arguments: Map.put(query.arguments, argument.name, value)}
          add_invalid_errors(query, argument, "is invalid")
      end
    else
      %{query | arguments: Map.put(query.arguments, argument, value)}
    end
  end

  defp add_invalid_errors(query, argument, error) do
    messages =
      if Keyword.keyword?(error) do
        [error]
      else
        List.wrap(error)
      end

    messages
    |> Enum.reduce(query, fn message, query ->
      message
      |> Ash.Changeset.error_to_exception_opts(argument)
      |> Enum.reduce(query, fn opts, query ->
        add_error(query, InvalidArgument.exception(opts))
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

  @doc """
  Builds a query from a keyword list.

  This is used by certain query constructs like aggregates. It can also be used to manipulate a data structure
  before passing it to an ash query. It allows for building an entire query struct using only a keyword list.

  For example:

  ```elixir
  Ash.Query.build(MyResource, filter: [name: "fred"], sort: [name: :asc], load: [:foo, :bar], offset: 10)
  ```

  If you want to use the expression style filters, you can use `expr/1`. Be sure to `require Ash.Query` first,
  or import it. Consider importing only the `expr/1` macro if you do that

  For example:

  ```elixir
  import Ash.Query, only: [expr: 1]

  Ash.Query.build(Myresource, filter: expr(name == "marge"))
  ```

  Supported keys:
  * `filter` - filter keyword/expr or `%Ash.Filter{}`
  * `sort` - sort keyword
  * `limit` - integer limit
  * `offset` - integer offset
  * `load` - keyword/list of atoms to load
  * `aggregate` - `{name, type, relationship}`
  * `aggregate` - `{name, type, relationship, query_in_build_format}`
  * `calculate` - `{name, module_and_opts}`
  * `calculate` - `{name, module_and_opts, context}`
  * `distinct` - list of atoms
  * `context: %{key: value}`
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

      {:aggregate, {name, type, relationship}}, query ->
        aggregate(query, name, type, relationship)

      {:aggregate, {name, type, relationship, agg_query}}, query ->
        aggregate(query, name, type, relationship, agg_query)

      {:calculate, {name, module_and_opts}}, query ->
        calculate(query, name, module_and_opts)

      {:calculate, {name, module_and_opts, context}}, query ->
        calculate(query, name, module_and_opts, context)

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
  """
  @spec aggregate(
          t() | Ash.Resource.t(),
          atom(),
          Ash.Query.Aggregate.kind(),
          atom | list(atom),
          Keyword.t() | nil
        ) :: t()
  def aggregate(query, name, type, relationship, agg_query \\ nil) do
    {field, agg_query} = Keyword.pop(agg_query || [], :field)

    query = to_query(query)
    relationship = List.wrap(relationship)

    if Ash.DataLayer.data_layer_can?(query.resource, {:aggregate, type}) do
      agg_query =
        case agg_query do
          [] ->
            nil

          options when is_list(options) ->
            build(Ash.Resource.Info.related(query.resource, relationship), options)
        end

      case Aggregate.new(query.resource, name, type, relationship, agg_query, field) do
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
        AggregatesNotSupported.exception(resource: query.resource, feature: "using")
      )
    end
  end

  @doc """
  Adds a calculation to the query.

  Calculations are made available on the `calculations` field of the records returned

  The `module_and_opts` argument accepts either a `module` or a `{module, opts}`. For more information
  on what that module should look like, see `Ash.Calculation`.

  More features for calculations, like passing anonymous functions, will be supported in the future.
  """
  def calculate(query, name, module_and_opts, context \\ %{}) do
    query = to_query(query)

    {module, opts} =
      case module_and_opts do
        {module, opts} -> {module, opts}
        module -> {module, []}
      end

    case Calculation.new(name, module, opts, context) do
      {:ok, calculation} ->
        %{query | calculations: Map.put(query.calculations, name, calculation)}

      {:error, error} ->
        add_error(query, :calculations, error)
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
        Enum.reduce(errors, query, &add_error(&2, :load, &1))
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

      _errors ->
        [
          {:error,
           InvalidQuery.exception(
             query: load_query,
             load_path: Enum.reverse(path)
           )}
        ]
    end
  end

  defp do_validate_load(query, {atom, _} = tuple, path) when is_atom(atom) do
    do_validate_load(query, [tuple], path)
  end

  defp do_validate_load(query, loads, path) when is_list(loads) do
    loads
    |> List.wrap()
    |> Enum.flat_map(fn
      {key, value} ->
        case Ash.Resource.Info.relationship(query.resource, key) do
          nil ->
            [
              {:error,
               NoSuchRelationship.exception(
                 resource: query.resource,
                 relationship: key,
                 load_path: Enum.reverse(path)
               )}
            ]

          relationship ->
            cond do
              !selecting?(query, relationship.source_field) ->
                [
                  {:error,
                   "Cannot load a relationship if you are not selecting the source field of that relationship"}
                ]

              !Ash.Resource.Info.primary_action(relationship.destination, :read) ->
                [
                  {:error,
                   NoReadAction.exception(
                     resource: relationship.destination,
                     when: "loading relationship #{relationship.name}"
                   )}
                ]

              relationship.type == :many_to_many &&
                  !Ash.Resource.Info.primary_action(relationship.through, :read) ->
                [
                  {:error,
                   NoReadAction.exception(
                     resource: relationship.destination,
                     when: "loading relationship #{relationship.name}"
                   )}
                ]

              match?(%Ash.Query{}, value) ->
                validate_matching_query_and_continue(
                  value,
                  query.resource,
                  key,
                  path,
                  relationship
                )

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
            {:ok, filter}

          existing_filter ->
            Ash.Filter.add_to_filter(existing_filter, filter, :and, query.aggregates)
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
      agg_names =
        query.resource
        |> Ash.Resource.Info.aggregates()
        |> Enum.map(& &1.name)

      temp_query = Ash.Query.load(query, agg_names)

      filter =
        if query.filter do
          Ash.Filter.add_to_filter(query.filter, statement, :and, query.aggregates)
        else
          Ash.Filter.parse(query.resource, statement, temp_query.aggregates)
        end

      case filter do
        {:ok, filter} ->
          aggregates_to_load =
            filter
            |> Ash.Filter.used_aggregates()
            |> Enum.map(& &1.name)
            |> Enum.filter(&(&1 in agg_names))
            |> Enum.reject(&Map.has_key?(query.aggregates, &1))

          query
          |> Ash.Query.load(aggregates_to_load)
          |> Map.put(:filter, filter)

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  @doc """
  Sort the results based on attributes or aggregates (calculations are not yet supported)

  Takes a list of fields to sort on, or a keyword list/mixed keyword list of fields and sort directions.
  The default sort direction is `:asc`.

  Examples:

  ```
  Ash.Query.sort(query, [:foo, :bar])

  Ash.Query.sort(query, [:foo, bar: :desc])

  Ash.Query.sort(query, [foo: :desc, bar: :asc])
  ```
  """
  @spec sort(t() | Ash.Resource.t(), Ash.Sort.t()) :: t()
  def sort(query, sorts) do
    query = to_query(query)

    if Ash.DataLayer.data_layer_can?(query.resource, :sort) do
      sorts
      |> List.wrap()
      |> Enum.reduce(query, fn
        {sort, direction}, query ->
          %{query | sort: query.sort ++ [{sort, direction}]}

        sort, query ->
          %{query | sort: query.sort ++ [{sort, :asc}]}
      end)
      |> validate_sort()
    else
      add_error(query, :sort, "Data layer does not support sorting")
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

    keys
    |> Enum.reduce(query, fn key, query ->
      if key in [:api, :resource] do
        query
      else
        struct(query, [{key, Map.get(%__MODULE__{}, key)}])
      end
    end)
  end

  def unset(query, key) do
    if key in [:api, :resource] do
      to_query(query)
    else
      query
      |> to_query()
      |> struct([{key, Map.get(%__MODULE__{}, key)}])
    end
  end

  @doc "Return the underlying data layer query for an ash query"
  def data_layer_query(%{resource: resource, api: api} = ash_query, opts \\ []) do
    query = Ash.DataLayer.resource_to_query(resource, api)

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
           add_aggregates(query, ash_query, aggregates),
         {:ok, query} <-
           Ash.DataLayer.sort(query, ash_query.sort, resource),
         {:ok, query} <- maybe_filter(query, ash_query, opts),
         {:ok, query} <- add_tenant(query, ash_query),
         {:ok, query} <-
           Ash.DataLayer.limit(query, ash_query.limit, resource),
         {:ok, query} <-
           Ash.DataLayer.offset(query, ash_query.offset, resource) do
      Ash.DataLayer.set_context(resource, query, ash_query.context)
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

    aggregates
    |> Enum.map(&add_tenant_to_aggregate_query(&1, ash_query))
    |> Enum.reduce_while({:ok, query}, fn aggregate, {:ok, query} ->
      case Ash.DataLayer.add_aggregate(query, aggregate, resource) do
        {:ok, query} -> {:cont, {:ok, query}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp add_tenant_to_aggregate_query(aggregate, %{tenant: nil}), do: aggregate

  defp add_tenant_to_aggregate_query(%{query: nil} = aggregate, ash_query) do
    aggregate_with_query = %{aggregate | query: Ash.Query.new(aggregate.resource)}
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
    case Sort.process(resource, sort, query.aggregates) do
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
        %{query | errors: [error | query.errors]}
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

  @spec to_query(t() | Ash.Resource.t()) :: t()
  defp to_query(%__MODULE__{} = query), do: query

  defp to_query(resource) do
    resource
    |> new()
    |> Ash.DataLayer.transform_query()
  end

  defp merge_load([], right), do: sanitize_loads(right)
  defp merge_load(left, []), do: sanitize_loads(left)

  defp merge_load(
         %__MODULE__{load: left_loads, tenant: left_tenant},
         %__MODULE__{load: right_loads} = query
       ) do
    %{query | load: merge_load(left_loads, right_loads)}
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
