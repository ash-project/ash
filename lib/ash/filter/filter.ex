defmodule Ash.Filter do
  # credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
  @dialyzer {:nowarn_function, do_map: 2, map: 2}
  alias Ash.Actions.Load
  alias Ash.Engine.Request

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    CalculationsNotSupported,
    InvalidFilterValue,
    NoSuchAttributeOrRelationship,
    NoSuchFilterPredicate,
    NoSuchFunction,
    NoSuchOperator,
    ReadActionRequired
  }

  alias Ash.Error.Invalid.InvalidPrimaryKey

  alias Ash.Query.Function.{Ago, Contains, If, IsNil}

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    Has,
    In,
    LessThan,
    LessThanOrEqual,
    NotEq
  }

  alias Ash.Query.{BooleanExpression, Call, Not, Ref}
  alias Ash.Query.{Aggregate, Calculation, Function, Operator}

  @functions [
    Ago,
    Contains,
    IsNil,
    If
  ]

  @operators [
               Ash.Query.Operator.IsNil,
               Eq,
               NotEq,
               In,
               LessThan,
               GreaterThan,
               LessThanOrEqual,
               GreaterThanOrEqual,
               Has
             ] ++ Ash.Query.Operator.Basic.operator_modules()

  @builtins @functions ++ @operators

  @operators_with_aliases @operators |> Enum.reject(&(&1.name() == &1.operator()))

  @operator_aliases [
                      equals: Eq,
                      not_equals: NotEq,
                      gt: GreaterThan,
                      lt: LessThan,
                      gte: GreaterThanOrEqual,
                      lte: LessThanOrEqual
                    ] ++ Enum.map(@operators_with_aliases, &{&1.name(), &1})

  @moduledoc """
  The representation of a filter in Ash.

  Ash filters are stored as nested `Ash.Query.BooleanExpression{}` and `%Ash.Query.Not{}` structs,
  terminating in an operator or a function struct. An expression is simply a boolean operator
  and the left and right hand side of that operator.

  ## Filter Templates

  Filter templates are simplified fielter statements (they only support atom keys), that have substitutions in them.
  Currently, the substitutions are `{:_actor, :field}` and `{:_actor, :_primary_key}`

  You can pass a filter template to `build_filter_from_template/2` with an actor, and it will return the new result

  Additionally, you can ask if the filter template contains an actor reference via `template_references_actor?/1`

  ## Writing a filter

  ### Built In Predicates

  #{Enum.map_join(@operators, "\n", &"* `#{&1.operator()}`")}
  #{Enum.map_join(@operator_aliases, "\n", fn {key, val} -> "* `#{key}` (alias for `#{val.operator()}`)" end)}

  ### BooleanExpression syntax

  The expression syntax ultimately just builds the keyword list style filter, but with lots of conveniences that
  would be very annoying to do manually.

  Examples

  ```elixir
  Ash.Query.filter(resource, name == "Zardoz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name == "Doz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name in ["Doz", "Daz"] and high_score > 10)
  Ash.Query.filter(resource, first_name == "Zar" or last_name == "Doz" or (high_score > 10 and high_score < -10))
  ```

  ### Keyword list syntax

  A filter is a nested keyword list (with some exceptions, like `true` for everything and `false` for nothing).

  The key is the "predicate" (A.K.A condition) and the value is the parameter. You can use `and` and `or` to create
  nested filters. Datalayers can expose custom predicates. Eventually, you will be able to define your own custom
  predicates, which will be a mechanism for you to attach complex filters supported by the data layer to your queries.

  ** Important **
  In a given keyword list, all predicates are considered to be "ands". So `[or: [first_name: "Tom", last_name: "Bombadil"]]` doesn't
  mean 'First name == "tom" or last_name == "bombadil"'. To say that, you want to provide a list of filters,
  like so: `[or: [[first_name: "Tom"], [last_name: "Bombadil"]]]`

  Some example filters:

  ```elixir
  Ash.Query.filter(resource, [name: "Zardoz"]))
  Ash.Query.filter(resource, [first_name: "Zar", last_name: "Doz"])
  Ash.Query.filter(resource, [first_name: "Zar", last_name: [in: ["Doz", "Daz"]], high_score: [greater_than: 10]])
  Ash.Query.filter(resource, [or: [
    [first_name: "Zar"],
    [last_name: "Doz"],
    [or: [
      [high_score: [greater_than: 10]]],
      [high_score: [less_than: -10]]
    ]
  ]])

  ### Other formats

  Maps are also accepted, as are maps with string keys. Technically, a list of `[{"string_key", value}]` would also work.
  If you are using a map with string keys, it is likely that you are parsing input. It is important to note that, before
  passing a filter supplied from an external source directly to `Ash.Query.filter/2`, you should first call `Ash.Filter.parse_input/2`
  (or `Ash.Filter.parse_input/4` if your query has aggregates/calculations in it). This ensures that the filter only uses public attributes,
  relationships, aggregates and calculations. You may additionally wish to pass in the query context, in the case that you have calculations
  that use the provided context.
  ```
  """

  @builtin_operators Enum.map(@operators, &{&1.operator(), &1}) ++ @operator_aliases
  @builtin_functions Enum.map(@functions, &{&1.name(), &1})

  @string_builtin_operators Enum.into(@builtin_operators, %{}, fn {key, value} ->
                              {to_string(key), value}
                            end)

  @string_builtin_functions Enum.into(@builtin_functions, %{}, fn {key, value} ->
                              {to_string(key), value}
                            end)

  defstruct [:resource, :expression]

  @type t :: %__MODULE__{}

  def builtins, do: @builtins
  def builtin_functions, do: @functions
  def builtin_operators, do: @operators
  def builtin_predicate_operators, do: Enum.filter(@operators, & &1.predicate?())

  defmodule Simple do
    @moduledoc "Represents a simplified filter, with a simple list of predicates"
    defstruct [:resource, :predicates]

    defmodule Not do
      @moduledoc "A negated predicate"
      defstruct [:predicate]
    end
  end

  @doc """
  Parses a filter statement, accepting only public attributes/relationships

  See `parse/2` for more
  """
  def parse_input(
        resource,
        statement,
        aggregates \\ %{},
        calculations \\ %{},
        context \\ %{}
      ) do
    context = %{
      resource: resource,
      relationship_path: [],
      aggregates: aggregates,
      calculations: calculations,
      public?: true,
      data_layer: Ash.DataLayer.data_layer(resource),
      query_context: context
    }

    with {:ok, expression} <- parse_expression(statement, context),
         :ok <- validate_references(expression, resource) do
      {:ok, %__MODULE__{expression: expression, resource: resource}}
    end
  end

  @doc """
  Parses a filter statement, accepting only public attributes/relationships, raising on errors.

  See `parse_input/2` for more
  """
  def parse_input!(resource, statement, aggregates \\ %{}, calculations \\ %{}, context \\ %{}) do
    case parse_input(resource, statement, aggregates, calculations, context) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise error
    end
  end

  @doc """
  Parses a filter statement

  See `parse/2` for more
  """
  def parse!(resource, statement, aggregates \\ %{}, calculations \\ %{}, context \\ %{}) do
    case parse(resource, statement, aggregates, calculations, context) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise error
    end
  end

  @doc """
  Parses a filter statement

  See the module documentation for more information on the supported formats for filter
  statements.

  ### Important

  If you are trying to validate a filter supplied from an external/untrusted source,
  be sure to use `parse_input/2` instead! The only difference is that it only accepts
  filters over public attributes/relationships.

  ### Aggregates and calculations

  Since custom aggregates/calculations can be added to a query, and they must be explicitly loaded into
  a query, the filter parser does not parse them by default. If you wish to support parsing filters
  over aggregates/calculations, provide them as the third argument. The best way to do this is to build a query
  with them added/loaded, and then use the `aggregates` and `calculations` keys on the query.

  ### NOTE

  A change was made recently that will automatically load any aggregates/calculations that are used in a filter, but
  if you are using this function you still need to pass them in.

  ```elixir
  Ash.Filter.parse(MyResource, [id: 1], query.aggregates, query.calculations)
  ```
  """
  def parse(resource, statement, aggregates \\ %{}, calculations \\ %{}, context \\ %{})

  def parse(_resource, nil, _aggregates, _calculations, _context) do
    {:ok, nil}
  end

  def parse(resource, statement, aggregates, calculations, context) do
    context = %{
      resource: resource,
      relationship_path: [],
      aggregates: aggregates,
      calculations: calculations,
      public?: false,
      data_layer: Ash.DataLayer.data_layer(resource),
      query_context: context
    }

    with {:ok, expression} <- parse_expression(statement, context),
         :ok <- validate_references(expression, resource) do
      {:ok, %__MODULE__{expression: expression, resource: resource}}
    end
  end

  defp validate_references(expression, resource) do
    refs =
      expression
      |> list_refs()
      |> Enum.map(fn ref ->
        field =
          case ref.attribute do
            field when is_atom(field) ->
              Ash.Resource.Info.field(resource, field)

            field ->
              field
          end

        %{ref | attribute: field}
      end)

    errors =
      refs
      |> Enum.flat_map(fn ref ->
        field = ref.attribute

        # This handles manually added calcualtions and aggregates
        case Map.fetch(field, :filterable?) do
          :error ->
            []

          {:ok, true} ->
            []

          {:ok, false} ->
            [Ash.Error.Query.InvalidFilterReference.exception(field: field.name)]

          {:ok, :simple_equality} ->
            if ref.simple_equality? do
              []
            else
              [
                Ash.Error.Query.InvalidFilterReference.exception(
                  field: field.name,
                  simple_equality?: true
                )
              ]
            end
        end
      end)

    multiple_filter_errors =
      refs
      |> Enum.filter(fn ref ->
        Map.fetch(ref.attribute, :filterable?) == {:ok, :simple_equality}
      end)
      |> Enum.group_by(& &1.attribute.name)
      |> Enum.flat_map(fn
        {_, []} ->
          []

        {_, [_]} ->
          []

        {name, _} ->
          [
            Ash.Error.Query.InvalidFilterReference.exception(
              field: name,
              simple_equality?: true
            )
          ]
      end)

    case Enum.concat(errors, multiple_filter_errors) do
      [] ->
        :ok

      errors ->
        {:error, Enum.uniq(errors)}
    end
  end

  @doc """
  Returns a filter statement that would find a single record based on the input.

  For example:

      iex> get_filter(MyApp.Post, 1)
      {:ok, %{id: 1}} #using primary key
      iex> get_filter(MyApp.Post, id: 1)
      {:ok, %{id: 1}} #using primary key
      iex> get_filter(MyApp.Post, author_id: 1, publication_id: 2, first_name: "fred")
      {:ok, %{author_id: 1, publication_id: 1}} # using a unique identity
      iex> get_filter(MyApp.Post, first_name: "fred")
      :error # not enough information
  """
  def get_filter(resource, id) do
    primary_key = Ash.Resource.Info.primary_key(resource)
    keyval? = Keyword.keyword?(id) || is_map(id)

    case {primary_key, id} do
      {[field], [{field, value}]} ->
        {:ok, %{field => value}}

      {[field], value} when not keyval? ->
        {:ok, %{field => value}}

      {fields, value} ->
        if keyval? do
          with :error <- get_keys(value, fields),
               :error <- get_identity_filter(resource, id) do
            {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
          end
        else
          {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
        end
    end
  end

  defp get_keys(value, fields) do
    Enum.reduce_while(fields, {:ok, %{}}, fn field, {:ok, vals} ->
      case fetch(value, field) do
        {:ok, value} ->
          {:cont, {:ok, Map.put(vals, field, value)}}

        :error ->
          case fetch(value, to_string(field)) do
            {:ok, value} ->
              {:cont, {:ok, Map.put(vals, field, value)}}

            :error ->
              {:halt, :error}
          end
      end
    end)
  end

  defp fetch(val, key) when is_map(val), do: Map.fetch(val, key)
  defp fetch(val, key) when is_list(val) and is_atom(key), do: Keyword.fetch(val, key)
  defp fetch(_, _), do: :error

  defp get_identity_filter(resource, id) do
    resource
    |> Ash.Resource.Info.identities()
    |> Enum.find_value(
      :error,
      fn identity ->
        case get_keys(id, identity.keys) do
          {:ok, key} ->
            {:ok, key}

          _ ->
            nil
        end
      end
    )
  end

  @to_simple_filter_options [
    skip_invalid?: [
      type: :boolean,
      default: false,
      doc:
        "If an invalid filter expression is reached that can't be used with a simple filter (like an `or` statement, or a non-predicate expression), it will be ignored instead of raising an error."
    ]
  ]

  @doc """
  Transform an expression based filter to a simple filter, which is just a list of predicates

  Options:

    - skip_invalid?:
  """
  def to_simple_filter(%{resource: resource, expression: expression}, opts \\ []) do
    opts = NimbleOptions.validate!(opts, @to_simple_filter_options)
    predicates = get_predicates(expression, opts[:skip_invalid?])

    %Simple{resource: resource, predicates: predicates}
  end

  @doc "Replace any actor value references in a template with the values from a given actor"
  def build_filter_from_template(template, actor \\ nil, args \\ %{}, context \\ %{}) do
    walk_filter_template(template, fn
      {:_actor, :_primary_key} ->
        if actor do
          Map.take(actor, Ash.Resource.Info.primary_key(actor.__struct__))
        else
          false
        end

      {:_actor, field} ->
        Map.get(actor || %{}, field)

      {:_arg, field} ->
        Map.get(args, field) || Map.get(args, to_string(field))

      {:_context, fields} when is_list(fields) ->
        get_in(context, fields)

      {:_context, field} ->
        Map.get(context, field)

      other ->
        other
    end)
  end

  @doc "Whether or not a given template contains an actor reference"
  def template_references_actor?({:_actor, _}), do: true

  def template_references_actor?(%BooleanExpression{op: :and, left: left, right: right}) do
    template_references_actor?(left) || template_references_actor?(right)
  end

  def template_references_actor?(%Not{expression: expression}) do
    template_references_actor?(expression)
  end

  def template_references_actor?(%{left: left, right: right}) do
    template_references_actor?(left) || template_references_actor?(right)
  end

  def template_references_actor?(%{arguments: args}) do
    Enum.any?(args, &template_references_actor?/1)
  end

  def template_references_actor?(%Ash.Query.Call{args: args}) do
    Enum.any?(args, &template_references_actor?/1)
  end

  def template_references_actor?(_), do: false

  defp walk_filter_template(filter, mapper) when is_list(filter) do
    case mapper.(filter) do
      ^filter ->
        Enum.map(filter, &walk_filter_template(&1, mapper))

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(%BooleanExpression{left: left, right: right} = expr, mapper) do
    case mapper.(expr) do
      ^expr ->
        %{
          expr
          | left: walk_filter_template(left, mapper),
            right: walk_filter_template(right, mapper)
        }

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(%Not{expression: expression} = not_expr, mapper) do
    case mapper.(not_expr) do
      ^not_expr ->
        %{not_expr | expression: walk_filter_template(expression, mapper)}

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(%{__predicate__?: _, left: left, right: right} = pred, mapper) do
    case mapper.(pred) do
      ^pred ->
        %{
          pred
          | left: walk_filter_template(left, mapper),
            right: walk_filter_template(right, mapper)
        }

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(%{__predicate__?: _, arguments: arguments} = func, mapper) do
    case mapper.(func) do
      ^func ->
        %{
          func
          | arguments: Enum.map(arguments, &walk_filter_template(&1, mapper))
        }

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(%Call{args: args} = call, mapper) do
    case mapper.(call) do
      ^call ->
        %{
          call
          | args: Enum.map(args, &walk_filter_template(&1, mapper))
        }

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(filter, mapper) when is_map(filter) do
    if Map.has_key?(filter, :__struct__) do
      filter
    else
      case mapper.(filter) do
        ^filter ->
          Enum.into(filter, %{}, &walk_filter_template(&1, mapper))

        other ->
          walk_filter_template(other, mapper)
      end
    end
  end

  defp walk_filter_template(tuple, mapper) when is_tuple(tuple) do
    case mapper.(tuple) do
      ^tuple ->
        tuple
        |> Tuple.to_list()
        |> Enum.map(&walk_filter_template(&1, mapper))
        |> List.to_tuple()

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(value, mapper), do: mapper.(value)

  @doc """
  Can be used to find a simple equality predicate on an attribute

  Use this when your attribute is configured with `filterable? :simple_equality`, and you want to
  to find the value that it is being filtered on with (if any).
  """
  def find_simple_equality_predicate(expression, attribute) do
    expression
    |> find(&simple_eq?(&1, attribute))
    |> case do
      nil ->
        nil

      %{right: right} ->
        right
    end
  end

  defp simple_eq?(%Eq{left: %Ref{}, right: %Ref{}}, _), do: false

  defp simple_eq?(%Eq{right: %Ref{}} = eq, attribute) do
    simple_eq?(%{eq | left: eq.right, right: eq.left}, attribute)
  end

  defp simple_eq?(%Eq{left: %Ref{attribute: attribute}}, attribute), do: true
  defp simple_eq?(%Eq{left: %Ref{attribute: %{name: attribute}}}, attribute), do: true
  defp simple_eq?(_, _), do: false

  @doc "Find an expression inside of a filter that matches the provided predicate"
  def find(expr, pred) do
    if pred.(expr) do
      expr
    else
      case expr do
        %__MODULE__{expression: expression} ->
          find(expression, pred)

        %Not{expression: expression} ->
          find(expression, pred)

        %BooleanExpression{left: left, right: right} ->
          find(left, pred) || find(right, pred)

        %Call{args: arguments} ->
          Enum.find(arguments, &find(&1, pred))

        %{__operator__?: true, left: left, right: right} ->
          find(left, pred) || find(right, pred)

        %{__function__?: true, arguments: arguments} ->
          Enum.find(arguments, &find(&1, pred))

        _ ->
          nil
      end
    end
  end

  defp get_predicates(expr, skip_invalid?, acc \\ [])

  defp get_predicates(true, _skip_invalid?, acc), do: acc
  defp get_predicates(false, _, _), do: false
  defp get_predicates(_, _, false), do: false

  defp get_predicates(%BooleanExpression{op: :and, left: left, right: right}, skip_invalid?, acc) do
    acc = get_predicates(left, skip_invalid?, acc)
    get_predicates(right, skip_invalid?, acc)
  end

  defp get_predicates(%Not{expression: expression}, skip_invalid?, acc) do
    expression
    |> get_predicates(skip_invalid?)
    |> Enum.reduce(acc, fn predicate, acc ->
      [%Simple.Not{predicate: predicate} | acc]
    end)
  end

  defp get_predicates(%{__predicate__?: true} = predicate, _skip_invalid?, acc),
    do: [predicate | acc]

  defp get_predicates(_invalid, true, acc), do: acc

  defp get_predicates(invalid, false, _acc) do
    raise "Invalid filter statement provided: #{inspect(invalid)} while constructing a simple filter. To skip invalid statements, use `skip_invalid?: true`."
  end

  def used_calculations(
        filter,
        resource,
        relationship_path \\ [],
        calculations \\ %{},
        aggregates \\ %{}
      ) do
    filter
    |> list_refs()
    |> Enum.filter(fn
      %Ref{attribute: %Calculation{}, relationship_path: ref_relationship_path} ->
        (relationship_path in [nil, []] and ref_relationship_path in [nil, []]) ||
          relationship_path == ref_relationship_path

      _ ->
        false
    end)
    |> Enum.map(& &1.attribute)
    |> calculations_used_by_calculations(
      resource,
      relationship_path,
      calculations,
      aggregates
    )
  end

  defp calculations_used_by_calculations(
         used_calculations,
         resource,
         relationship_path,
         calculations,
         aggregates
       ) do
    used_calculations
    |> Enum.flat_map(fn calculation ->
      expression = calculation.module.expression(calculation.opts, calculation.context)

      case Ash.Filter.hydrate_refs(expression, %{
             resource: resource,
             aggregates: aggregates,
             calculations: calculations,
             public?: false
           }) do
        {:ok, expression} ->
          with_recursive_used =
            calculations_used_by_calculations(
              used_calculations(
                expression,
                resource,
                relationship_path,
                calculations,
                aggregates
              ),
              resource,
              relationship_path,
              calculations,
              aggregates
            )

          [calculation | with_recursive_used]

        _ ->
          [calculation]
      end
    end)
  end

  def used_aggregates(filter, relationship_path \\ [], return_refs? \\ false) do
    refs =
      filter
      |> list_refs()
      |> Enum.filter(fn
        %Ref{attribute: %Aggregate{}, relationship_path: ref_relationship_path} ->
          relationship_path == :all ||
            (relationship_path in [nil, []] and ref_relationship_path in [nil, []]) ||
            relationship_path == ref_relationship_path

        _ ->
          false
      end)

    if return_refs? do
      refs
    else
      Enum.map(refs, & &1.attribute)
    end
  end

  def put_at_path(value, []), do: value
  def put_at_path(value, [key | rest]), do: [{key, put_at_path(value, rest)}]

  def add_to_filter!(
        base,
        addition,
        op \\ :and,
        aggregates \\ %{},
        calculations \\ %{},
        context \\ %{}
      ) do
    case add_to_filter(base, addition, op, aggregates, calculations, context) do
      {:ok, value} ->
        value

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  def add_to_filter(
        base,
        addition,
        op \\ :and,
        aggregates \\ %{},
        calculations \\ %{},
        context \\ %{}
      )

  def add_to_filter(nil, %__MODULE__{} = addition, _, _, _, _), do: {:ok, addition}

  def add_to_filter(
        %__MODULE__{} = base,
        %__MODULE__{} = addition,
        op,
        _,
        _,
        _
      ) do
    {:ok,
     %{
       base
       | expression: BooleanExpression.optimized_new(op, base.expression, addition.expression)
     }}
  end

  def add_to_filter(%__MODULE__{} = base, statement, op, aggregates, calculations, context) do
    case parse(base.resource, statement, aggregates, calculations, context) do
      {:ok, filter} -> add_to_filter(base, filter, op, aggregates, calculations)
      {:error, error} -> {:error, error}
    end
  end

  @doc """
  Returns true if the second argument is a strict subset (always returns the same or less data) of the first
  """
  def strict_subset_of(nil, _), do: true

  def strict_subset_of(_, nil), do: false

  def strict_subset_of(%{resource: resource}, %{resource: other_resource})
      when resource != other_resource,
      do: false

  def strict_subset_of(filter, candidate) do
    Ash.SatSolver.strict_filter_subset(filter, candidate)
  end

  def strict_subset_of?(filter, candidate) do
    strict_subset_of(filter, candidate) == true
  end

  def relationship_filter_request_paths(filter) do
    filter
    |> relationship_paths()
    |> Enum.map(&[:filter, &1])
  end

  def read_requests(_, nil, _), do: {:ok, []}

  def read_requests(api, %{resource: original_resource} = filter, request_path) do
    filter
    |> Ash.Filter.relationship_paths()
    |> Enum.map(fn path ->
      {path, scope_expression_by_relationship_path(filter, path)}
    end)
    |> Enum.reduce_while({:ok, []}, fn {path, scoped_filter}, {:ok, requests} ->
      %{resource: resource} = scoped_filter

      with %{errors: []} = query <- Ash.Query.new(resource, api),
           %{errors: []} = query <- Ash.Query.do_filter(query, scoped_filter),
           {:action, action} when not is_nil(action) <-
             {:action, Ash.Resource.Info.primary_action(resource, :read)} do
        request =
          Request.new(
            resource: resource,
            api: api,
            query:
              Request.resolve(
                [request_path ++ [:fetch, :authorization_filter]],
                fn context ->
                  authorization_filter =
                    get_in(context, request_path ++ [:fetch, :authorization_filter])

                  if authorization_filter do
                    relationship =
                      Ash.Resource.Info.relationship(
                        original_resource,
                        List.first(path)
                      )

                    case Load.reverse_relationship_path(
                           relationship,
                           tl(path)
                         ) do
                      :error ->
                        {:ok, query}

                      {:ok, reverse_relationship} ->
                        filter = put_at_path(authorization_filter, reverse_relationship)
                        {:ok, Ash.Query.do_filter(query, filter)}
                    end
                  else
                    {:ok, query}
                  end
                end
              ),
            async?: false,
            path: request_path ++ [:filter, path],
            strict_check_only?: true,
            action: action,
            name: "authorize filter #{Enum.join(path, ".")}",
            data: []
          )

        {:cont, {:ok, [request | requests]}}
      else
        {:error, error} -> {:halt, {:error, error}}
        %{errors: errors} -> {:halt, {:error, errors}}
        {:action, nil} -> {:halt, {:error, ReadActionRequired.exception(resource: resource)}}
      end
    end)
  end

  defp map(%__MODULE__{expression: nil} = filter, _) do
    filter
  end

  defp map(%__MODULE__{expression: expression} = filter, func) do
    %{filter | expression: do_map(func.(expression), func)}
  end

  defp map(expression, func) do
    do_map(func.(expression), func)
  end

  defp do_map(expression, func) do
    case expression do
      {:halt, expr} ->
        expr

      %BooleanExpression{left: left, right: right} = expr ->
        %{expr | left: do_map(left, func), right: do_map(right, func)}

      %Not{expression: not_expr} = expr ->
        %{expr | expression: do_map(not_expr, func)}

      %{__operator__?: true, left: left, right: right} = op ->
        %{op | left: do_map(left, func), right: do_map(right, func)}

      %{__function__?: true, arguments: arguments} = func ->
        %{
          func
          | arguments:
              Enum.map(arguments, fn
                {key, arg} when is_atom(key) ->
                  {key, do_map(arg, func)}

                arg ->
                  do_map(arg, func)
              end)
        }

      other ->
        func.(other)
    end
  end

  def update_aggregates(%__MODULE__{expression: expression} = filter, mapper) do
    %{filter | expression: update_aggregates(expression, mapper)}
  end

  def update_aggregates(expression, mapper) do
    case expression do
      {key, value} when is_atom(key) ->
        {key, update_aggregates(value, mapper)}

      %Not{expression: expression} = not_expr ->
        %{not_expr | expression: update_aggregates(expression, mapper)}

      %BooleanExpression{left: left, right: right} = expression ->
        %{
          expression
          | left: update_aggregates(left, mapper),
            right: update_aggregates(right, mapper)
        }

      %{__operator__?: true, left: left, right: right} = op ->
        left = update_aggregates(left, mapper)
        right = update_aggregates(right, mapper)
        %{op | left: left, right: right}

      %{__function__?: true, arguments: args} = func ->
        %{func | arguments: Enum.map(args, &update_aggregates(&1, mapper))}

      %Ref{attribute: %Aggregate{} = agg} = ref ->
        %{ref | attribute: mapper.(agg, ref)}

      other ->
        other
    end
  end

  def run_other_data_layer_filters(api, resource, %{expression: expression} = filter, data) do
    case do_run_other_data_layer_filters(expression, api, resource, data) do
      {:filter_requests, requests} -> {:filter_requests, requests}
      {:ok, new_expression} -> {:ok, %{filter | expression: new_expression}}
      {:error, error} -> {:error, error}
    end
  end

  def run_other_data_layer_filters(_, _, filter, _data) when filter in [nil, true, false],
    do: {:ok, filter}

  defp do_run_other_data_layer_filters(
         %BooleanExpression{op: :or, left: left, right: right},
         api,
         resource,
         data
       ) do
    left_result = do_run_other_data_layer_filters(left, api, resource, data)
    right_result = do_run_other_data_layer_filters(right, api, resource, data)

    case {left_result, right_result} do
      {{:ok, left}, {:ok, right}} ->
        {:ok, BooleanExpression.optimized_new(:or, left, right)}

      {{:error, error}, _} ->
        {:error, error}

      {_, {:error, error}} ->
        {:error, error}

      {{:filter_requests, left_filter_requests}, {:filter_requests, right_filter_requests}} ->
        {:filter_requests, left_filter_requests ++ right_filter_requests}

      {{:filter_requests, left_filter_requests}, _} ->
        {:filter_requests, left_filter_requests}

      {_, {:filter_requests, right_filter_requests}} ->
        {:filter_requests, right_filter_requests}
    end
  end

  defp do_run_other_data_layer_filters(
         %BooleanExpression{op: :and} = expression,
         api,
         resource,
         data
       ) do
    expression
    |> relationship_paths(:ands_only)
    |> filter_paths_that_change_data_layers(resource)
    |> case do
      [] ->
        {:ok, expression}

      paths ->
        paths
        |> do_run_other_data_layer_filter_paths(expression, resource, api, data)
        |> case do
          {:filter_requests, requests} -> {:filter_requests, requests}
          {:ok, result} -> do_run_other_data_layer_filters(result, api, resource, data)
          {:error, error} -> {:error, error}
        end
    end
  end

  defp do_run_other_data_layer_filters(%Not{expression: expression}, api, resource, data) do
    case do_run_other_data_layer_filters(expression, api, resource, data) do
      {:ok, expr} -> {:ok, Not.new(expr)}
      {:filter_requests, requests} -> {:filter_requests, requests}
      {:error, error} -> {:error, error}
    end
  end

  defp do_run_other_data_layer_filters(%{__predicate__?: _} = predicate, api, resource, data) do
    predicate
    |> relationship_paths(:ands_only)
    |> filter_paths_that_change_data_layers(resource)
    |> Enum.find_value(fn path ->
      case split_expression_by_relationship_path(predicate, path) do
        {nil, _} ->
          nil

        {for_path, nil} ->
          {path, for_path}
      end
    end)
    |> case do
      nil ->
        {:ok, predicate}

      {path, new_predicate} ->
        relationship = Ash.Resource.Info.relationship(resource, path)

        fetch_related_data(resource, path, new_predicate, api, relationship, data)
    end
  end

  defp do_run_other_data_layer_filters(other, _api, _resource, _data), do: {:ok, other}

  defp do_run_other_data_layer_filter_paths(paths, expression, resource, api, data) do
    Enum.reduce_while(paths, {:ok, expression, []}, fn path, {:ok, expression, requests} ->
      {for_path, without_path} = split_expression_by_relationship_path(expression, path)

      relationship = Ash.Resource.Info.relationship(resource, path)

      query =
        relationship.destination
        |> Ash.Query.new(api)
        |> Map.put(:filter, %__MODULE__{
          expression: for_path,
          resource: relationship.destination
        })

      case filter_related_in(query, relationship, :lists.droplast(path), api, data) do
        {:ok, new_predicate} ->
          if requests == [] do
            {:cont, {:ok, BooleanExpression.optimized_new(:and, without_path, new_predicate), []}}
          else
            {:cont, {:ok, new_predicate, []}}
          end

        {:filter_requests, new_requests} ->
          {:ok, expression, requests ++ new_requests}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, expr, []} ->
        {:ok, expr}

      {:ok, _, requests} ->
        {:filter_requests, requests}

      other ->
        other
    end
  end

  defp split_expression_by_relationship_path(%{expression: expression}, path) do
    split_expression_by_relationship_path(expression, path)
  end

  defp split_expression_by_relationship_path(
         %BooleanExpression{op: op, left: left, right: right},
         path
       ) do
    {new_for_path_left, new_without_path_left} = split_expression_by_relationship_path(left, path)

    {new_for_path_right, new_without_path_right} =
      split_expression_by_relationship_path(right, path)

    {BooleanExpression.optimized_new(op, new_for_path_left, new_for_path_right),
     BooleanExpression.optimized_new(op, new_without_path_left, new_without_path_right)}
  end

  defp split_expression_by_relationship_path(%Not{expression: expression}, path) do
    {new_for_path, new_without_path} = split_expression_by_relationship_path(expression, path)
    {Not.new(new_for_path), Not.new(new_without_path)}
  end

  defp split_expression_by_relationship_path(
         %{
           __predicate__?: _,
           left: left,
           right: right
         } = predicate,
         path
       ) do
    refs = list_refs([left, right])

    if Enum.any?(refs, &List.starts_with?(&1.relationship_path, path)) do
      if Enum.all?(refs, &List.starts_with?(&1.relationship_path, path)) do
        {scope_refs(predicate, path), nil}
      else
        {scope_refs(predicate, path), predicate}
      end
    else
      {nil, predicate}
    end
  end

  defp split_expression_by_relationship_path(
         %{__predicate__?: _, arguments: args} = predicate,
         path
       ) do
    refs = list_refs(args)

    if Enum.any?(refs, &List.starts_with?(&1.relationship_path, path)) do
      if Enum.all?(refs, &List.starts_with?(&1.relationship_path, path)) do
        {scope_refs(predicate, path), nil}
      else
        {scope_refs(predicate, path), predicate}
      end
    else
      {nil, predicate}
    end
  end

  defp scope_refs(%BooleanExpression{left: left, right: right} = expr, path) do
    %{expr | left: scope_refs(left, path), right: scope_refs(right, path)}
  end

  defp scope_refs(%Not{expression: expression} = expr, path) do
    %{expr | expression: scope_refs(expression, path)}
  end

  defp scope_refs(%{__predicate__?: _, left: left, right: right} = pred, path) do
    %{pred | left: scope_refs(left, path), right: scope_refs(right, path)}
  end

  defp scope_refs(%{__predicate__?: _, argsuments: arguments} = pred, path) do
    %{pred | args: Enum.map(arguments, &scope_refs(&1, path))}
  end

  defp scope_refs({key, value}, path) do
    {key, scope_refs(value, path)}
  end

  defp scope_refs(%Ref{relationship_path: ref_path} = ref, path) do
    if List.starts_with?(ref_path, path) do
      %{ref | relationship_path: Enum.drop(ref_path, Enum.count(path))}
    else
      ref
    end
  end

  defp scope_refs(other, _), do: other

  def prefix_refs(%BooleanExpression{left: left, right: right} = expr, path) do
    %{expr | left: prefix_refs(left, path), right: prefix_refs(right, path)}
  end

  def prefix_refs(%Not{expression: expression} = expr, path) do
    %{expr | expression: prefix_refs(expression, path)}
  end

  def prefix_refs(%{__predicate__?: _, left: left, right: right} = pred, path) do
    %{pred | left: prefix_refs(left, path), right: prefix_refs(right, path)}
  end

  def prefix_refs(%{__predicate__?: _, argsuments: arguments} = pred, path) do
    %{pred | args: Enum.map(arguments, &prefix_refs(&1, path))}
  end

  def prefix_refs(%Ref{relationship_path: ref_path} = ref, path) do
    if List.starts_with?(ref_path, path) do
      %{ref | relationship_path: path ++ ref_path}
    else
      ref
    end
  end

  def prefix_refs(other, _), do: other

  defp fetch_related_data(
         resource,
         path,
         new_predicate,
         api,
         %{type: :many_to_many, join_relationship: join_relationship, through: through} =
           relationship,
         data
       ) do
    if Ash.DataLayer.data_layer(through) == Ash.DataLayer.data_layer(resource) &&
         Ash.DataLayer.data_layer_can?(resource, {:join, through}) do
      filter = %__MODULE__{
        resource: relationship.destination,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(api)
      |> Ash.Query.do_filter(filter)
      |> filter_related_in(
        relationship,
        :lists.droplast(path) ++ [join_relationship],
        api,
        data
      )
    else
      filter = %__MODULE__{
        resource: through,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(api)
      |> Ash.Query.do_filter(filter)
      |> Ash.Actions.Read.unpaginated_read()
      |> case do
        {:ok, results} ->
          relationship.through
          |> Ash.Query.new(api)
          |> Ash.Query.do_filter([
            {relationship.destination_field_on_join_table,
             in: Enum.map(results, &Map.get(&1, relationship.destination_field))}
          ])
          |> filter_related_in(
            Ash.Resource.Info.relationship(resource, join_relationship),
            :lists.droplast(path),
            api,
            data
          )

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp fetch_related_data(
         _resource,
         path,
         new_predicate,
         api,
         relationship,
         data
       ) do
    filter = %__MODULE__{
      resource: relationship.destination,
      expression: new_predicate
    }

    relationship.destination
    |> Ash.Query.new(api)
    |> Ash.Query.do_filter(filter)
    |> Ash.Query.do_filter(relationship.filter)
    |> Ash.Query.sort(relationship.sort)
    |> Ash.Query.set_context(relationship.context)
    |> filter_related_in(relationship, :lists.droplast(path), api, data)
  end

  defp filter_related_in(query, relationship, path, api, {request_path, tenant, data}) do
    query = Ash.Query.set_tenant(query, tenant)
    path = request_path ++ [:other_data_layer_filter, path ++ [relationship.name], query]

    case get_in(data, path ++ [:data]) do
      %{data: records} ->
        records_to_expression(
          records,
          relationship,
          path
        )

      _ ->
        action = Ash.Resource.Info.primary_action!(query.resource, :read)
        action = %{action | pagination: false}

        {:filter_requests,
         Ash.Actions.Read.as_requests(path, query.resource, api, action,
           query: query,
           page: false,
           tenant: tenant
         )
         |> Enum.map(fn request ->
           # By returning the request and a key, we register a dependency on that key
           {request, :data}
         end)}
    end
  end

  defp records_to_expression([], _, _), do: {:ok, false}

  defp records_to_expression([single_record], relationship, path) do
    Ash.Query.Operator.new(
      Eq,
      %Ref{
        relationship_path: path,
        resource: relationship.source,
        attribute: Ash.Resource.Info.attribute(relationship.source, relationship.source_field)
      },
      Map.get(single_record, relationship.destination_field)
    )
  end

  defp records_to_expression(records, relationship, path) do
    Enum.reduce_while(records, {:ok, nil}, fn record, {:ok, expression} ->
      case records_to_expression([record], relationship, path) do
        {:ok, operator} ->
          {:cont, {:ok, BooleanExpression.optimized_new(:and, expression, operator)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp filter_paths_that_change_data_layers(paths, resource, acc \\ [])
  defp filter_paths_that_change_data_layers([], _resource, acc), do: acc

  defp filter_paths_that_change_data_layers([path | rest], resource, acc) do
    case shortest_path_to_changed_data_layer(resource, path) do
      {:ok, path} ->
        new_rest = Enum.reject(rest, &List.starts_with?(&1, path))
        filter_paths_that_change_data_layers(new_rest, resource, [path | acc])

      :error ->
        filter_paths_that_change_data_layers(rest, resource, acc)
    end
  end

  defp shortest_path_to_changed_data_layer(resource, path, acc \\ [])
  defp shortest_path_to_changed_data_layer(_resource, [], _acc), do: :error

  defp shortest_path_to_changed_data_layer(resource, [relationship | rest], acc) do
    relationship = Ash.Resource.Info.relationship(resource, relationship)

    if relationship.type == :many_to_many do
      if Ash.DataLayer.data_layer_can?(resource, {:join, relationship.through}) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    else
      if Ash.DataLayer.data_layer_can?(resource, {:join, relationship.destination}) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    end
  end

  def relationship_paths(filter_or_expression, kind \\ :all)
  def relationship_paths(nil, _), do: []
  def relationship_paths(%{expression: nil}, _), do: []

  def relationship_paths(%__MODULE__{expression: expression}, kind),
    do: relationship_paths(expression, kind)

  def relationship_paths(expression, kind) do
    expression
    |> do_relationship_paths(kind)
    |> List.wrap()
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.map(fn {path} -> path end)
  end

  defp do_relationship_paths(%Ref{relationship_path: path}, _) when path != [] do
    {path}
  end

  defp do_relationship_paths(%BooleanExpression{op: :or}, :ands_only) do
    []
  end

  defp do_relationship_paths(%BooleanExpression{left: left, right: right}, kind) do
    [do_relationship_paths(left, kind), do_relationship_paths(right, kind)]
  end

  defp do_relationship_paths(%Not{expression: expression}, kind) do
    do_relationship_paths(expression, kind)
  end

  defp do_relationship_paths(%{__operator__?: true, left: left, right: right}, kind) do
    [do_relationship_paths(left, kind), do_relationship_paths(right, kind)]
  end

  defp do_relationship_paths({key, value}, kind) when is_atom(key) do
    do_relationship_paths(value, kind)
  end

  defp do_relationship_paths(%{__function__?: true, arguments: arguments}, kind) do
    Enum.map(arguments, &do_relationship_paths(&1, kind))
  end

  defp do_relationship_paths(_, _), do: []

  @doc false
  def embed_predicates(nil), do: nil

  def embed_predicates(%__MODULE__{expression: expression} = filter) do
    %{filter | expression: embed_predicates(expression)}
  end

  def embed_predicates(%Not{expression: expression} = not_expr) do
    %{not_expr | expression: embed_predicates(expression)}
  end

  def embed_predicates(%BooleanExpression{left: left, right: right} = expr) do
    %{expr | left: embed_predicates(left), right: embed_predicates(right)}
  end

  def embed_predicates(%Call{args: args} = call) do
    %{call | args: embed_predicates(args)}
  end

  def embed_predicates(%{__predicate__?: true} = pred) do
    %{pred | embedded?: true}
  end

  def embed_predicates(list) when is_list(list) do
    Enum.map(list, &embed_predicates(&1))
  end

  def embed_predicates(other), do: other

  def list_refs(expression, no_longer_simple? \\ false, in_an_eq? \\ false)

  def list_refs(list, no_longer_simple?, in_an_eq?) when is_list(list) do
    Enum.flat_map(list, &list_refs(&1, no_longer_simple?, in_an_eq?))
  end

  def list_refs({key, value}, no_longer_simple?, in_an_eq?) when is_atom(key),
    do: list_refs(value, no_longer_simple?, in_an_eq?)

  def list_refs(%__MODULE__{expression: expression}, no_longer_simple?, in_an_eq?) do
    list_refs(expression, no_longer_simple?, in_an_eq?)
  end

  def list_refs(expression, no_longer_simple?, in_an_eq?) do
    case expression do
      %BooleanExpression{left: left, right: right, op: op} ->
        no_longer_simple? = no_longer_simple? || op == :or
        list_refs(left, no_longer_simple?) ++ list_refs(right, no_longer_simple?)

      %Not{expression: not_expr} ->
        list_refs(not_expr, true)

      %struct{__predicate__?: _, left: left, right: right} ->
        in_an_eq? = struct == Ash.Query.Operator.Eq

        list_refs(left, no_longer_simple?, in_an_eq?) ++
          list_refs(right, no_longer_simple?, in_an_eq?)

      %{__predicate__?: _, arguments: args} ->
        Enum.flat_map(args, &list_refs(&1, true))

      %Call{args: args} ->
        Enum.flat_map(args, &list_refs(&1, true))

      %Ref{} = ref ->
        [%{ref | simple_equality?: !no_longer_simple? && in_an_eq?}]

      _ ->
        []
    end
  end

  def list_predicates(%__MODULE__{expression: expression}) do
    list_predicates(expression)
  end

  def list_predicates(expression) do
    case expression do
      %BooleanExpression{left: left, right: right} ->
        list_predicates(left) ++ list_predicates(right)

      %Not{expression: not_expr} ->
        list_predicates(not_expr)

      %{__predicate__?: true} = pred ->
        [pred]

      _ ->
        []
    end
  end

  def scope_expression_by_relationship_path(filter, path) do
    %__MODULE__{
      resource: Ash.Resource.Info.related(filter.resource, path),
      expression: do_scope_expression_by_relationship_path(filter.expression, path)
    }
  end

  defp do_scope_expression_by_relationship_path(
         %BooleanExpression{op: op, left: left, right: right},
         path
       ) do
    new_left = do_scope_expression_by_relationship_path(left, path)
    new_right = do_scope_expression_by_relationship_path(right, path)

    BooleanExpression.optimized_new(op, new_left, new_right)
  end

  defp do_scope_expression_by_relationship_path(%Not{expression: expression}, path) do
    new_expression = do_scope_expression_by_relationship_path(expression, path)
    Not.new(new_expression)
  end

  defp do_scope_expression_by_relationship_path(
         %{__operator__?: true, left: left, right: right} = op,
         path
       ) do
    [left, right] = Enum.map([left, right], &do_scope_expression_by_relationship_path(&1, path))
    %{op | left: left, right: right}
  end

  defp do_scope_expression_by_relationship_path(
         %{__function__?: true, arguments: arguments} = func,
         path
       ) do
    arguments = Enum.map(arguments, &do_scope_expression_by_relationship_path(&1, path))
    %{func | arguments: arguments}
  end

  defp do_scope_expression_by_relationship_path(%Call{args: arguments} = call, path) do
    arguments = Enum.map(arguments, &do_scope_expression_by_relationship_path(&1, path))
    %{call | args: arguments}
  end

  defp do_scope_expression_by_relationship_path({key, value}, path) do
    {key, do_scope_expression_by_relationship_path(value, path)}
  end

  defp do_scope_expression_by_relationship_path(%Ref{} = ref, path) do
    if List.starts_with?(ref.relationship_path, path) do
      %{ref | relationship_path: Enum.drop(ref.relationship_path, Enum.count(path))}
    else
      ref
    end
  end

  defp do_scope_expression_by_relationship_path(other, _path) do
    other
  end

  defp attribute(%{public?: true, resource: resource}, attribute),
    do: Ash.Resource.Info.public_attribute(resource, attribute)

  defp attribute(%{public?: false, resource: resource}, attribute),
    do: Ash.Resource.Info.attribute(resource, attribute)

  defp aggregate(%{public?: true, resource: resource}, aggregate),
    do: Ash.Resource.Info.public_aggregate(resource, aggregate)

  defp aggregate(%{public?: false, resource: resource}, aggregate),
    do: Ash.Resource.Info.aggregate(resource, aggregate)

  defp calculation(%{public?: true, resource: resource}, calculation),
    do: Ash.Resource.Info.public_calculation(resource, calculation)

  defp calculation(%{public?: false, resource: resource}, calculation),
    do: Ash.Resource.Info.calculation(resource, calculation)

  defp relationship(%{public?: true, resource: resource}, relationship) do
    Ash.Resource.Info.public_relationship(resource, relationship)
  end

  defp relationship(%{public?: false, resource: resource}, relationship) do
    Ash.Resource.Info.relationship(resource, relationship)
  end

  defp related(context, relationship) when not is_list(relationship) do
    related(context, [relationship])
  end

  defp related(context, []), do: context.resource

  defp related(context, [rel | rest]) do
    case relationship(context, rel) do
      %{destination: destination} -> related(%{context | resource: destination}, rest)
      nil -> nil
    end
  end

  defp parse_expression(%__MODULE__{expression: expression}, context),
    do: {:ok, move_to_relationship_path(expression, context.relationship_path)}

  defp parse_expression(statement, context) when is_list(statement) do
    Enum.reduce_while(statement, {:ok, nil}, fn expression_part, {:ok, expression} ->
      case add_expression_part(expression_part, context, expression) do
        {:ok, new_expression} ->
          {:cont, {:ok, new_expression}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_expression(statement, context) do
    parse_expression([statement], context)
  end

  defp add_expression_part(boolean, _context, expression) when is_boolean(boolean),
    do: {:ok, BooleanExpression.optimized_new(:and, expression, boolean)}

  defp add_expression_part(%__MODULE__{expression: adding_expression}, context, expression) do
    {:ok,
     BooleanExpression.optimized_new(
       :and,
       expression,
       move_to_relationship_path(adding_expression, context.relationship_path)
     )}
  end

  defp add_expression_part({not_key, nested_statement}, context, expression)
       when not_key in [:not, "not"] do
    case parse_expression(nested_statement, context) do
      {:ok, nested_expression} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, Not.new(nested_expression))}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({or_key, nested_statements}, context, expression)
       when or_key in [:or, "or"] do
    with {:ok, nested_expression} <- parse_and_join(nested_statements, :or, context),
         :ok <- validate_data_layers_support_boolean_filters(nested_expression) do
      {:ok, BooleanExpression.optimized_new(:and, expression, nested_expression)}
    end
  end

  defp add_expression_part({and_key, nested_statements}, context, expression)
       when and_key in [:and, "and"] do
    case parse_and_join(nested_statements, :and, context) do
      {:ok, nested_expression} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, nested_expression)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(%Call{} = call, context, expression) do
    case resolve_call(call, context) do
      {:ok, result} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, result)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({%Ref{} = ref, nested_statement}, context, expression) do
    case related(context, ref.relationship_path) do
      nil ->
        {:error,
         NoSuchAttributeOrRelationship.exception(
           attribute_or_relationship: List.first(ref.relationship_path),
           resource: context.resource
         )}

      related ->
        new_context = %{
          relationship_path: ref.relationship_path,
          resource: related,
          aggregates: context.aggregates,
          calculations: context.calculations,
          query_context: context.query_context,
          public?: context.public?
        }

        add_expression_part({ref.attribute.name, nested_statement}, new_context, expression)
    end
  end

  defp add_expression_part(
         %BooleanExpression{op: op, left: left, right: right},
         context,
         expression
       ) do
    add_expression_part({op, [left, right]}, context, expression)
  end

  defp add_expression_part(%Not{expression: not_expression}, context, expression) do
    add_expression_part({:not, not_expression}, context, expression)
  end

  defp add_expression_part(%_{} = record, context, expression) do
    pkey_filter =
      record
      |> Map.take(Ash.Resource.Info.primary_key(context.resource))
      |> Map.to_list()

    add_expression_part(pkey_filter, context, expression)
  end

  defp add_expression_part({:is_nil, attribute}, context, expression) when is_atom(attribute) do
    add_expression_part({attribute, [is_nil: true]}, context, expression)
  end

  defp add_expression_part({function, args}, context, expression)
       when is_tuple(args) and is_atom(function) do
    case get_function(function, context.resource) do
      nil ->
        {:error,
         NoSuchAttributeOrRelationship.exception(
           attribute_or_relationship: function,
           resource: context.resource
         )}

      function_module ->
        nested_statement = Tuple.to_list(args)

        with {:ok, args} <-
               hydrate_refs(List.wrap(nested_statement), context),
             refs <- list_refs(args),
             :ok <-
               validate_not_crossing_datalayer_boundaries(
                 refs,
                 context.resource,
                 {function, nested_statement}
               ),
             {:ok, function} <-
               Function.new(
                 function_module,
                 args
               ) do
          if is_boolean(function) do
            {:ok, BooleanExpression.optimized_new(:and, expression, function)}
          else
            if Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
              {:ok, BooleanExpression.optimized_new(:and, expression, function)}
            else
              {:error, "data layer does not support the function #{inspect(function)}"}
            end
          end
        end
    end
  end

  defp add_expression_part({field, nested_statement}, context, expression)
       when is_atom(field) or is_binary(field) do
    aggregates =
      Enum.flat_map(context.aggregates, fn {key, _} ->
        [key, to_string(key)]
      end)

    calculations =
      Enum.flat_map(context.calculations, fn {key, _} ->
        [key, to_string(key)]
      end)

    cond do
      rel = relationship(context, field) ->
        context =
          context
          |> Map.update!(:relationship_path, fn path -> path ++ [rel.name] end)
          |> Map.put(:resource, rel.destination)
          |> Map.update!(
            :query_context,
            &Ash.Helpers.deep_merge_maps(&1 || %{}, rel.context || %{})
          )

        if is_list(nested_statement) || is_map(nested_statement) do
          case parse_expression(nested_statement, context) do
            {:ok, nested_expression} ->
              {:ok, BooleanExpression.optimized_new(:and, expression, nested_expression)}

            {:error, error} ->
              {:error, error}
          end
        else
          with [field] <- Ash.Resource.Info.primary_key(context.resource),
               attribute <- attribute(context, field),
               {:ok, casted} <-
                 Ash.Type.cast_input(attribute.type, nested_statement, attribute.constraints) do
            add_expression_part({field, casted}, context, expression)
          else
            _other ->
              {:error,
               InvalidFilterValue.exception(
                 value: inspect(nested_statement),
                 message:
                   "A single value must be castable to the primary key of the resource: #{inspect(context.resource)}"
               )}
          end
        end

      attr = attribute(context, field) ->
        case parse_predicates(nested_statement, attr, context) do
          {:ok, nested_statement} ->
            {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

          {:error, error} ->
            {:error, error}
        end

      aggregate = aggregate(context, field) ->
        related = Ash.Resource.Info.related(context.resource, aggregate.relationship_path)

        with %{valid?: true} = aggregate_query <-
               Ash.Query.build(related, filter: aggregate.filter, sort: aggregate.sort),
             {:ok, query_aggregate} <-
               Aggregate.new(
                 context.resource,
                 aggregate.name,
                 aggregate.kind,
                 aggregate.relationship_path,
                 aggregate_query,
                 aggregate.field,
                 aggregate.default,
                 aggregate.filterable?
               ) do
          case parse_predicates(nested_statement, query_aggregate, context) do
            {:ok, nested_statement} ->
              {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

            {:error, error} ->
              {:error, error}
          end
        else
          %{valid?: false, errors: errors} ->
            {:error, errors}

          {:error, error} ->
            {:error, error}
        end

      field in calculations ->
        {module, _} = module_and_opts(Map.get(context.calculations, field).calculation)

        field =
          if is_binary(field) do
            String.to_existing_atom(field)
          else
            field
          end

        add_calculation_expression(context, nested_statement, field, module, expression)

      field in aggregates ->
        field =
          if is_binary(field) do
            String.to_existing_atom(field)
          else
            field
          end

        add_aggregate_expression(context, nested_statement, field, expression)

      resource_calculation = calculation(context, field) ->
        {module, opts} = module_and_opts(resource_calculation.calculation)

        {input, nested_statement} =
          case nested_statement do
            {input, nested} ->
              {input || %{}, nested}

            nested ->
              {%{}, nested}
          end

        with {:ok, args} <-
               Ash.Query.validate_calculation_arguments(
                 resource_calculation,
                 input
               ),
             {:ok, calculation} <-
               Calculation.new(
                 resource_calculation.name,
                 module,
                 opts,
                 resource_calculation.type,
                 Map.put(args, :context, context.query_context),
                 resource_calculation.filterable?
               ) do
          case parse_predicates(nested_statement, calculation, context) do
            {:ok, nested_statement} ->
              {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

            {:error, error} ->
              {:error, error}
          end
        else
          {:error, error} ->
            {:error, error}
        end

      op_module = get_operator(field) && match?([_, _ | _], nested_statement) ->
        with {:ok, [left, right]} <-
               hydrate_refs(nested_statement, context),
             refs <- list_refs([left, right]),
             :ok <-
               validate_not_crossing_datalayer_boundaries(
                 refs,
                 context.resource,
                 {field, nested_statement}
               ),
             {:ok, operator} <- Operator.new(op_module, left, right) do
          if is_boolean(operator) do
            {:ok, BooleanExpression.optimized_new(:and, expression, operator)}
          else
            if Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
              {:ok, BooleanExpression.optimized_new(:and, expression, operator)}
            else
              {:error, "data layer does not support the operator #{inspect(operator)}"}
            end
          end
        end

      true ->
        {:error,
         NoSuchAttributeOrRelationship.exception(
           attribute_or_relationship: field,
           resource: context.resource
         )}
    end
  end

  defp add_expression_part(value, context, expression) when is_map(value) do
    # Can't call `parse_expression/2` here because it will loop

    value
    |> Map.to_list()
    |> Enum.reduce_while({:ok, nil}, fn {key, value}, {:ok, expression} ->
      case add_expression_part({key, value}, context, expression) do
        {:ok, new_expression} ->
          {:cont, {:ok, new_expression}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, new_expression} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, new_expression)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(value, context, expression) when is_list(value) do
    Enum.reduce_while(value, {:ok, expression}, fn value, {:ok, expression} ->
      case add_expression_part(value, context, expression) do
        {:ok, expression} -> {:cont, {:ok, expression}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp add_expression_part(value, _, _) do
    {:error, InvalidFilterValue.exception(value: value)}
  end

  defp validate_not_crossing_datalayer_boundaries(refs, resource, expr) do
    refs
    |> Enum.map(&Ash.Resource.Info.related(resource, &1.relationship_path))
    |> Enum.filter(& &1)
    |> Enum.group_by(&Ash.DataLayer.data_layer/1)
    |> Map.to_list()
    |> case do
      [] ->
        :ok

      [{_data_layer, resources}] ->
        can_join? =
          Enum.all?(resources, fn resource ->
            resources
            |> Kernel.--([resource])
            |> Enum.all?(fn other_resource ->
              Ash.DataLayer.data_layer_can?(resource, {:join, other_resource})
            end)
          end)

        if can_join? do
          :ok
        else
          {:error,
           Ash.Error.Query.InvalidExpression.exception(
             expression: expr,
             message:
               "Cannot access multiple resources for a data layer that can't be joined from within a single expression"
           )}
        end

      [_ | _] ->
        {:error,
         Ash.Error.Query.InvalidExpression.exception(
           expression: expr,
           message: "Cannot access multiple data layers within a single expression"
         )}
    end
  end

  defp resolve_call(%Call{name: name, args: args, operator?: true} = call, context) do
    with :ok <- validate_datalayer_supports_nested_expressions(args, context.resource),
         {:op, op_module} when not is_nil(op_module) <-
           {:op, get_operator(name)},
         {:ok, [left, right]} <-
           hydrate_refs(args, context),
         refs <- list_refs([left, right]),
         :ok <-
           validate_not_crossing_datalayer_boundaries(refs, context.resource, call),
         {:ok, operator} <- Operator.new(op_module, left, right) do
      if is_boolean(operator) do
        {:ok, operator}
      else
        if Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
          {:ok, operator}
        else
          {:error, "data layer does not support the operator #{inspect(operator)}"}
        end
      end
    else
      {:op, nil} ->
        {:error, NoSuchOperator.exception(name: name)}

      other ->
        other
    end
  end

  defp resolve_call(%Call{name: name, args: args} = call, context) do
    could_be_calculation? = Enum.count(args) == 1 && Keyword.keyword?(Enum.at(args, 0))

    resource = Ash.Resource.Info.related(context.resource, call.relationship_path)

    case {calculation(%{context | resource: resource}, name), could_be_calculation?} do
      {resource_calculation, true} when not is_nil(resource_calculation) ->
        {module, opts} = module_and_opts(resource_calculation.calculation)

        with {:ok, args} <-
               Ash.Query.validate_calculation_arguments(
                 resource_calculation,
                 Map.new(Enum.at(args, 0) || [])
               ),
             {:ok, calculation} <-
               Calculation.new(
                 resource_calculation.name,
                 module,
                 opts,
                 resource_calculation.type,
                 Map.put(args, :context, context.query_context),
                 resource_calculation.filterable?
               ) do
          {:ok,
           %Ref{
             attribute: calculation,
             relationship_path: context.relationship_path ++ call.relationship_path,
             resource: resource
           }}
        else
          {:error, error} ->
            {:error, error}
        end

      _ ->
        with :ok <- validate_datalayer_supports_nested_expressions(args, context.resource),
             {:ok, args} <-
               hydrate_refs(args, context),
             refs <- list_refs(args),
             :ok <- validate_not_crossing_datalayer_boundaries(refs, context.resource, call),
             {:func, function_module} when not is_nil(function_module) <-
               {:func, get_function(name, context.resource)},
             {:ok, function} <-
               Function.new(
                 function_module,
                 args
               ) do
          if is_boolean(function) do
            {:ok, function}
          else
            if Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
              {:ok, function}
            else
              {:error, "data layer does not support the function #{inspect(function)}"}
            end
          end
        else
          {:func, nil} ->
            {:error, NoSuchFunction.exception(name: name, resource: context.resource)}

          other ->
            other
        end
    end
  end

  defp validate_datalayer_supports_nested_expressions(args, resource) do
    if Enum.any?(args, &Ash.Query.is_expr?/1) &&
         !Ash.DataLayer.data_layer_can?(resource, :nested_expressions) do
      {:error, "Datalayer does not support nested expressions"}
    else
      :ok
    end
  end

  defp module_and_opts({module, opts}), do: {module, opts}
  defp module_and_opts(module), do: {module, []}

  def hydrate_refs({key, value}, context) when is_atom(key) do
    case hydrate_refs(value, context) do
      {:ok, hydrated} ->
        {:ok, {key, hydrated}}

      other ->
        other
    end
  end

  def hydrate_refs(
        %Ref{attribute: attribute} = ref,
        %{aggregates: aggregates, calculations: calculations} = context
      )
      when is_atom(attribute) do
    case related(context, ref.relationship_path) do
      nil ->
        {:error,
         "Invalid reference #{inspect(ref)} at relationship_path #{inspect(ref.relationship_path)}"}

      related ->
        context = %{context | resource: related}

        cond do
          Map.has_key?(aggregates, attribute) ->
            {:ok, %{ref | attribute: Map.get(aggregates, attribute), resource: related}}

          Map.has_key?(calculations, attribute) ->
            {:ok, %{ref | attribute: Map.get(calculations, attribute), resource: related}}

          attribute = attribute(context, attribute) ->
            {:ok, %{ref | attribute: attribute, resource: related}}

          resource_calculation = calculation(context, attribute) ->
            {module, opts} = module_and_opts(resource_calculation.calculation)

            with {:ok, args} <-
                   Ash.Query.validate_calculation_arguments(resource_calculation, %{}),
                 {:ok, calculation} <-
                   Calculation.new(
                     resource_calculation.name,
                     module,
                     opts,
                     resource_calculation.type,
                     Map.put(args, :context, context.query_context),
                     resource_calculation.filterable?
                   ) do
              {:ok, %{ref | attribute: calculation, resource: related}}
            else
              {:error, error} ->
                {:error, error}
            end

          aggregate = aggregate(context, attribute) ->
            agg_related = Ash.Resource.Info.related(related, aggregate.relationship_path)

            with %{valid?: true} = aggregate_query <-
                   Ash.Query.build(agg_related, filter: aggregate.filter, sort: aggregate.sort),
                 {:ok, query_aggregate} <-
                   Aggregate.new(
                     related,
                     aggregate.name,
                     aggregate.kind,
                     aggregate.relationship_path,
                     aggregate_query,
                     aggregate.field,
                     aggregate.default,
                     aggregate.filterable?
                   ) do
              {:ok, %{ref | attribute: query_aggregate, resource: related}}
            else
              %{valid?: false, errors: errors} ->
                {:error, errors}

              {:error, error} ->
                {:error, error}
            end

          relationship = relationship(context, attribute) ->
            case Ash.Resource.Info.primary_key(relationship.destination) do
              [key] ->
                new_ref = %{
                  ref
                  | relationship_path: ref.relationship_path ++ [relationship.name],
                    attribute: Ash.Resource.Info.attribute(relationship.destination, key),
                    resource: relationship.destination
                }

                {:ok, new_ref}

              _ ->
                {:error,
                 "Invalid reference #{inspect(ref)} when hydrating relationship ref for #{inspect(ref.relationship_path ++ [relationship.name])}. Require single attribute primary key."}
            end

          true ->
            {:error, "Invalid reference #{inspect(ref)}"}
        end
    end
  end

  def hydrate_refs(%Ref{relationship_path: relationship_path, resource: nil} = ref, context) do
    {:ok, %{ref | resource: Ash.Resource.Info.related(context.resource, relationship_path)}}
  end

  def hydrate_refs(%BooleanExpression{left: left, right: right} = expr, context) do
    with {:ok, left} <- hydrate_refs(left, context),
         {:ok, right} <- hydrate_refs(right, context) do
      {:ok, %{expr | left: left, right: right}}
    else
      other ->
        other
    end
  end

  def hydrate_refs(%Not{expression: expression} = expr, context) do
    with {:ok, expression} <- hydrate_refs(expression, context) do
      {:ok, %{expr | expression: expression}}
    end
  end

  def hydrate_refs(%Call{} = call, context) do
    resolve_call(call, context)
  end

  def hydrate_refs(%{__predicate__?: _, left: left, right: right} = expr, context) do
    with {:ok, left} <- hydrate_refs(left, context),
         {:ok, right} <- hydrate_refs(right, context) do
      {:ok, %{expr | left: left, right: right}}
    else
      other ->
        other
    end
  end

  def hydrate_refs(%{__predicate__?: _, arguments: arguments} = expr, context) do
    case hydrate_refs(arguments, context) do
      {:ok, args} ->
        {:ok, %{expr | arguments: args}}

      other ->
        other
    end
  end

  def hydrate_refs(list, context) when is_list(list) do
    list
    |> Enum.reduce_while({:ok, []}, fn val, {:ok, acc} ->
      case hydrate_refs(val, context) do
        {:ok, value} ->
          {:cont, {:ok, [value | acc]}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, value} -> {:ok, Enum.reverse(value)}
      {:error, error} -> {:error, error}
    end
  end

  def hydrate_refs(val, _context) do
    {:ok, val}
  end

  defp add_aggregate_expression(context, nested_statement, field, expression) do
    if Ash.DataLayer.data_layer_can?(context.resource, :aggregate_filter) do
      case parse_predicates(nested_statement, Map.get(context.aggregates, field), context) do
        {:ok, nested_statement} ->
          {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, AggregatesNotSupported.exception(resource: context.resource, feature: "filtering")}
    end
  end

  defp add_calculation_expression(context, nested_statement, field, module, expression) do
    if Ash.DataLayer.data_layer_can?(context.resource, :expression_calculation) &&
         :erlang.function_exported(module, :expression, 2) do
      case parse_predicates(nested_statement, Map.get(context.calculations, field), context) do
        {:ok, nested_statement} ->
          {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:error,
       CalculationsNotSupported.exception(resource: context.resource, feature: "filtering")}
    end
  end

  defp validate_data_layers_support_boolean_filters(%BooleanExpression{
         op: :or,
         left: left,
         right: right
       }) do
    left_resources =
      left
      |> map(fn
        %Ref{} = ref ->
          [ref.resource]

        _ ->
          []
      end)
      |> List.flatten()
      |> Enum.uniq()

    right_resources =
      right
      |> map(fn
        %Ref{} = ref ->
          [ref.resource]

        _ ->
          []
      end)
      |> List.flatten()
      |> Enum.uniq()

    left_resources
    |> Enum.filter(&(&1 in right_resources))
    |> Enum.reduce_while(:ok, fn resource, :ok ->
      if Ash.DataLayer.data_layer_can?(resource, :boolean_filter) do
        {:cont, :ok}
      else
        {:halt, {:error, "Data layer for #{resource} does not support boolean filters"}}
      end
    end)
  end

  defp validate_data_layers_support_boolean_filters(_), do: :ok

  def move_to_relationship_path(expression, []), do: expression

  def move_to_relationship_path(expression, relationship_path) do
    case expression do
      {key, value} when is_atom(key) ->
        {key, move_to_relationship_path(value, relationship_path)}

      %Not{expression: expression} = not_expr ->
        %{not_expr | expression: move_to_relationship_path(expression, relationship_path)}

      %BooleanExpression{left: left, right: right} = expression ->
        %{
          expression
          | left: move_to_relationship_path(left, relationship_path),
            right: move_to_relationship_path(right, relationship_path)
        }

      %{__operator__?: true, left: left, right: right} = op ->
        left = move_to_relationship_path(left, relationship_path)
        right = move_to_relationship_path(right, relationship_path)
        %{op | left: left, right: right}

      %Ref{} = ref ->
        add_to_ref_path(ref, relationship_path)

      %{__function__?: true, arguments: args} = func ->
        %{func | arguments: Enum.map(args, &move_to_relationship_path(&1, relationship_path))}

      %Call{args: args} = call ->
        %{call | args: Enum.map(args, &move_to_relationship_path(&1, relationship_path))}

      %__MODULE__{expression: expression} = filter ->
        %{filter | expression: move_to_relationship_path(expression, relationship_path)}

      other ->
        other
    end
  end

  defp add_to_ref_path(%Ref{relationship_path: relationship_path} = ref, to_add) do
    %{ref | relationship_path: to_add ++ relationship_path}
  end

  defp add_to_ref_path(other, _), do: other

  defp parse_and_join(statements, op, context) do
    Enum.reduce_while(statements, {:ok, nil}, fn statement, {:ok, expression} ->
      case parse_expression(statement, context) do
        {:ok, nested_expression} ->
          {:cont, {:ok, BooleanExpression.optimized_new(op, expression, nested_expression)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_predicates(value, field, context) when not is_list(value) and not is_map(value) do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(values, attr, context) do
    if is_map(values) || Keyword.keyword?(values) do
      Enum.reduce_while(values, {:ok, nil}, fn
        {:not, value}, {:ok, expression} ->
          case parse_predicates(List.wrap(value), attr, context) do
            {:ok, not_expression} ->
              {:cont,
               {:ok,
                BooleanExpression.optimized_new(:and, expression, %Not{expression: not_expression})}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {key, value}, {:ok, expression} ->
          case get_operator(key) do
            nil ->
              error = NoSuchFilterPredicate.exception(key: key, resource: context.resource)
              {:halt, {:error, error}}

            operator_module ->
              left = %Ref{
                attribute: attr,
                relationship_path: context.relationship_path,
                resource: context.resource
              }

              with {:ok, [left, right]} <-
                     hydrate_refs([left, value], context),
                   refs <- list_refs([left, right]),
                   :ok <-
                     validate_not_crossing_datalayer_boundaries(
                       refs,
                       context.resource,
                       {attr, value}
                     ),
                   {:ok, operator} <- Operator.new(operator_module, left, right) do
                if is_boolean(operator) do
                  {:cont, {:ok, operator}}
                else
                  if Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
                    {:cont, {:ok, BooleanExpression.optimized_new(:and, expression, operator)}}
                  else
                    {:halt,
                     {:error, "data layer does not support the operator #{inspect(operator)}"}}
                  end
                end
              else
                {:error, error} -> {:halt, {:error, error}}
              end
          end
      end)
    else
      error = InvalidFilterValue.exception(value: values)
      {:error, error}
    end
  end

  def get_function(key, resource) when is_atom(key) do
    @builtin_functions[key] ||
      Enum.find(Ash.DataLayer.data_layer_functions(resource), &(&1.name() == key))
  end

  def get_function(key, resource) when is_binary(key) do
    Map.get(@string_builtin_functions, key) ||
      Enum.find(Ash.DataLayer.data_layer_functions(resource), &(&1.name() == key))
  end

  def get_function(_, _), do: nil

  def get_operator(key) when is_atom(key) do
    @builtin_operators[key]
  end

  def get_operator(key) when is_binary(key) do
    Map.get(@string_builtin_operators, key)
  end

  def get_operator(_), do: nil

  defimpl Inspect do
    import Inspect.Algebra

    @custom_colors [
      number: :cyan
    ]

    def inspect(
          %{expression: expression},
          opts
        ) do
      opts = %{opts | syntax_colors: Keyword.merge(opts.syntax_colors, @custom_colors)}
      concat(["#Ash.Filter<", to_doc(expression, opts), ">"])
    end
  end
end
