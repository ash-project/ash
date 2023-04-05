defmodule Ash.Filter do
  # credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
  @dialyzer {:nowarn_function, do_map: 2, map: 2}
  require Logger
  require Ash.Expr

  alias Ash.Engine.Request

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    CalculationsNotSupported,
    InvalidFilterValue,
    NoSuchAttributeOrRelationship,
    NoSuchFilterPredicate,
    NoSuchFunction,
    NoSuchOperator
  }

  alias Ash.Error.Invalid.InvalidPrimaryKey

  alias Ash.Query.Function.{
    Ago,
    Contains,
    DateAdd,
    DateTimeAdd,
    FromNow,
    GetPath,
    If,
    IsNil,
    Length,
    Now,
    StringJoin,
    Today,
    Type
  }

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
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
    DateAdd,
    DateTimeAdd,
    FromNow,
    GetPath,
    IsNil,
    If,
    Length,
    Now,
    Today,
    Type,
    StringJoin
  ]

  @operators [
               Ash.Query.Operator.IsNil,
               Eq,
               NotEq,
               In,
               LessThan,
               GreaterThan,
               LessThanOrEqual,
               GreaterThanOrEqual
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

  ## Filter Templates

  To see the available templates, see `Ash.Filter.TemplateHelpers`.
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

  The key is the "predicate" (or "condition") and the value is the parameter. You can use `and` and `or` to create
  nested filters. Data layers can expose custom predicates. Eventually, you will be able to define your own custom
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
  ```

  ### Other formats

  Maps are also accepted, as are maps with string keys. Technically, a list of `[{"string_key", value}]` would also work.
  If you are using a map with string keys, it is likely that you are parsing input. It is important to note that, before
  passing a filter supplied from an external source directly to `Ash.Query.filter/2`, you should first call `Ash.Filter.parse_input/2`
  (or `Ash.Filter.parse_input/4` if your query has aggregates/calculations in it). This ensures that the filter only uses public attributes,
  relationships, aggregates and calculations. You may additionally wish to pass in the query context, in the case that you have calculations
  that use the provided context.
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

  # Used for fetching related data in filters, which will have already had authorization rules applied
  defmodule ShadowApi do
    @moduledoc false
    use Ash.Api

    resources do
      allow_unregistered? true
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
    context =
      Map.merge(
        %{
          resource: resource,
          root_resource: resource,
          relationship_path: [],
          aggregates: aggregates,
          calculations: calculations,
          public?: true,
          data_layer: Ash.DataLayer.data_layer(resource)
        },
        context
      )

    with {:ok, expression} <- parse_expression(statement, context),
         {:ok, expression} <- hydrate_refs(expression, context),
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
        raise Ash.Error.to_error_class(error,
                error_context: parse_error_context(resource, statement, context)
              )
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

  def parse(resource, statement, aggregates, calculations, original_context) do
    context =
      Map.merge(
        %{
          resource: resource,
          relationship_path: [],
          aggregates: aggregates,
          calculations: calculations,
          public?: false,
          root_resource: resource,
          data_layer: Ash.DataLayer.data_layer(resource)
        },
        original_context
      )

    with {:ok, expression} <- parse_expression(statement, context),
         {:ok, expression} <- hydrate_refs(expression, context),
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
            field when is_atom(field) or is_binary(field) ->
              case Ash.Resource.Info.field(resource, field) do
                nil ->
                  field

                field ->
                  field
              end

            field ->
              field
          end

        %{ref | attribute: field}
      end)

    errors =
      refs
      |> Enum.flat_map(fn
        %{attribute: attribute, relationship_path: relationship_path}
        when is_atom(attribute) or is_binary(attribute) ->
          [
            NoSuchAttributeOrRelationship.exception(
              attribute_or_relationship: attribute,
              resource: Ash.Resource.Info.related(resource, relationship_path)
            )
          ]

        ref ->
          field = ref.attribute

          # This handles manually added calculations and aggregates
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
        is_map(ref.attribute) &&
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
        case cast_value(resource, field, value, id) do
          {:ok, value} ->
            {:ok, %{field => value}}

          {:error, error} ->
            {:error, error}
        end

      {[field], value} when not keyval? ->
        case cast_value(resource, field, value, id) do
          {:ok, value} ->
            {:ok, %{field => value}}

          {:error, error} ->
            {:error, error}
        end

      {fields, value} ->
        if keyval? do
          with :error <- get_keys(value, fields, resource),
               :error <- get_identity_filter(resource, id) do
            {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
          end
        else
          {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
        end
    end
  end

  defp get_keys(value, fields, resource) do
    original_value = value

    Enum.reduce_while(fields, {:ok, %{}}, fn field, {:ok, vals} ->
      case fetch(value, field) do
        {:ok, value} ->
          case cast_value(resource, field, value, original_value) do
            {:ok, value} ->
              {:cont, {:ok, Map.put(vals, field, value)}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        :error ->
          case fetch(value, to_string(field)) do
            {:ok, value} ->
              case cast_value(resource, field, value, original_value) do
                {:ok, value} ->
                  {:cont, {:ok, Map.put(vals, field, value)}}

                {:error, error} ->
                  {:error, error}
              end

            :error ->
              {:halt, :error}
          end
      end
    end)
  end

  defp cast_value(resource, field, value, id) do
    attribute = Ash.Resource.Info.attribute(resource, field)

    if attribute do
      case Ash.Type.cast_input(attribute.type, value, attribute.constraints) do
        {:ok, value} ->
          {:ok, value}

        _ ->
          {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
      end
    else
      {:error, InvalidPrimaryKey.exception(resource: resource, value: id)}
    end
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
        case get_keys(id, identity.keys, resource) do
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

      {:_actor, field} when is_atom(field) or is_binary(field) ->
        Map.get(actor || %{}, field)

      {:_actor, path} when is_list(path) ->
        get_path(actor || %{}, path)

      {:_arg, field} ->
        Map.get(args, field) || Map.get(args, to_string(field))

      {:_context, fields} when is_list(fields) ->
        get_path(context, fields)

      {:_context, field} ->
        Map.get(context, field)

      {:_ref, path, name} ->
        %Ref{
          attribute: name,
          relationship_path: path
        }

      other ->
        other
    end)
  end

  defp get_path(map, [key]) when is_struct(map) do
    Map.get(map, key)
  end

  defp get_path(map, [key]) when is_map(map) do
    Map.get(map, key)
  end

  defp get_path(map, [key | rest]) when is_map(map) do
    get_path(get_path(map, [key]), rest)
  end

  defp get_path(_, _), do: nil

  @doc "Whether or not a given template contains an actor reference"
  def template_references_actor?({:_actor, _}), do: true

  def template_references_actor?(%BooleanExpression{op: :and, left: left, right: right}) do
    template_references_actor?(left) || template_references_actor?(right)
  end

  def template_references_actor?(%Not{expression: expression}) do
    template_references_actor?(expression)
  end

  def template_references_actor?(%Ash.Query.Exists{expr: expr}) do
    template_references_actor?(expr)
  end

  def template_references_actor?(%Ash.Query.Parent{expr: expr}) do
    template_references_actor?(expr)
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

  def template_references_actor?(list) when is_list(list) do
    Enum.any?(list, &template_references_actor?/1)
  end

  def template_references_actor?(map) when is_map(map) and not is_struct(map) do
    Enum.any?(map, &template_references_actor?/1)
  end

  def template_references_actor?(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.any?(&template_references_actor?/1)
  end

  def template_references_actor?(_), do: false

  @doc false
  def walk_filter_template(filter, mapper) when is_list(filter) do
    case mapper.(filter) do
      ^filter ->
        Enum.map(filter, &walk_filter_template(&1, mapper))

      other ->
        walk_filter_template(other, mapper)
    end
  end

  def walk_filter_template(%BooleanExpression{left: left, right: right} = expr, mapper) do
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

  def walk_filter_template(%Not{expression: expression} = not_expr, mapper) do
    case mapper.(not_expr) do
      ^not_expr ->
        %{not_expr | expression: walk_filter_template(expression, mapper)}

      other ->
        walk_filter_template(other, mapper)
    end
  end

  def walk_filter_template(%Ash.Query.Parent{expr: expr} = this_expr, mapper) do
    case mapper.(this_expr) do
      ^this_expr ->
        %{this_expr | expr: walk_filter_template(expr, mapper)}

      other ->
        walk_filter_template(other, mapper)
    end
  end

  def walk_filter_template(%Ash.Query.Exists{expr: expr} = exists_expr, mapper) do
    case mapper.(exists_expr) do
      ^exists_expr ->
        %{exists_expr | expr: walk_filter_template(expr, mapper)}

      other ->
        walk_filter_template(other, mapper)
    end
  end

  def walk_filter_template(%{__predicate__?: _, left: left, right: right} = pred, mapper) do
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

  def walk_filter_template(%{__predicate__?: _, arguments: arguments} = func, mapper) do
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

  def walk_filter_template(%Call{args: args} = call, mapper) do
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

  def walk_filter_template(filter, mapper) when is_map(filter) do
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

  def walk_filter_template(tuple, mapper) when is_tuple(tuple) do
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

  def walk_filter_template(value, mapper), do: mapper.(value)

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

  def find_value(expr, pred) do
    do_find(expr, pred, true)
  end

  @doc "Find an expression inside of a filter that matches the provided predicate"
  def find(expr, pred) do
    do_find(expr, pred, false)
  end

  defp do_find(expr, pred, value?) do
    if value = pred.(expr) do
      if value? do
        value
      else
        expr
      end
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

      case hydrate_refs(expression, %{
             resource: resource,
             aggregates: aggregates,
             calculations: calculations,
             relationship_path: [],
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
    |> Enum.uniq()
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
        raise Ash.Error.to_ash_error(error, nil,
                error_context: parse_error_context(base.resource, addition, context)
              )
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

  defp parse_error_context(%{resource: resource} = _filter, addition, context) do
    parse_error_context(resource, addition, context)
  end

  defp parse_error_context(resource, addition, context) do
    context_str = if context, do: ", given context: #{inspect(context)}", else: ""

    "parsing addition of filter statement: #{inspect(addition)}, to resource: #{inspect(resource)}" <>
      context_str
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

  def read_requests(_, nil, _, _, _), do: {:ok, []}

  def read_requests(api, %{resource: resource} = filter, request_path, actor, tenant) do
    paths_with_refs = relationship_paths(filter, true, true)

    refs = group_refs_by_all_paths(paths_with_refs)

    paths_with_refs
    |> Enum.map(&elem(&1, 0))
    |> Enum.uniq()
    |> Enum.reject(&(&1 == []))
    |> Enum.reduce_while({:ok, []}, fn path, {:ok, requests} ->
      last_relationship =
        Enum.reduce(path, nil, fn
          relationship, nil ->
            Ash.Resource.Info.relationship(resource, relationship)

          relationship, acc ->
            Ash.Resource.Info.relationship(acc.destination, relationship)
        end)

      case relationship_query(resource, path, actor, tenant) do
        %{errors: []} = query ->
          request =
            Request.new(
              resource: query.resource,
              api: api,
              query:
                query
                |> Ash.Query.set_context(%{
                  accessing_from: %{
                    source: last_relationship.source,
                    name: last_relationship.name
                  }
                })
                |> Ash.Query.set_context(%{
                  filter_only?: true,
                  filter_references: refs[path] || []
                })
                |> Ash.Query.select([]),
              async?: false,
              path: request_path ++ [:filter, path],
              strict_check_only?: true,
              action: query.action,
              name: "authorize filter #{Enum.join(path, ".")}",
              data: []
            )

          {:cont, {:ok, [request | requests]}}

        %{errors: errors} ->
          {:halt, {:error, errors}}
      end
    end)
  end

  defp relationship_query(resource, [last], actor, tenant) do
    relationship = Ash.Resource.Info.relationship(resource, last)

    action =
      relationship.read_action ||
        Ash.Resource.Info.primary_action!(relationship.destination, :read).name

    relationship.destination
    |> Ash.Query.set_context(relationship.context)
    |> Ash.Query.sort(relationship.sort, prepend?: true)
    |> Ash.Query.do_filter(relationship.filter)
    |> Ash.Query.for_read(action, %{},
      actor: actor,
      authorize?: true,
      tenant: tenant
    )
  end

  defp relationship_query(resource, [next | rest], actor, tenant) do
    resource
    |> Ash.Resource.Info.related(next)
    |> relationship_query(rest, actor, tenant)
  end

  defp group_refs_by_all_paths(paths_with_refs) do
    all_paths_with_refs =
      paths_with_refs
      |> Enum.flat_map(fn {path, refs} ->
        Enum.map(refs, fn ref -> {path, ref} end)
      end)
      |> Enum.uniq()

    acc = %{
      [] => Enum.map(all_paths_with_refs, &elem(&1, 1))
    }

    Enum.reduce(all_paths_with_refs, acc, &add_ref_to_relevant_paths/2)
  end

  defp add_ref_to_relevant_paths(path_ref, acc, trail \\ [])

  defp add_ref_to_relevant_paths({[], ref}, acc, trail) do
    Map.update(acc, trail, [ref], &[ref | &1])
  end

  defp add_ref_to_relevant_paths({[next | rest], ref}, acc, trail) do
    new_trail = trail ++ [next]
    new_acc = Map.update(acc, new_trail, [ref], &[ref | &1])

    add_ref_to_relevant_paths({rest, ref}, new_acc, new_trail)
  end

  def map(%__MODULE__{expression: nil} = filter, _) do
    filter
  end

  def map(%__MODULE__{expression: expression} = filter, func) do
    %{filter | expression: do_map(func.(expression), func)}
  end

  def map(expression, func) do
    do_map(func.(expression), func)
  end

  defp do_map(expression, func) do
    case expression do
      {:halt, expr} ->
        expr

      %BooleanExpression{left: left, right: right} = expr ->
        %{expr | left: map(left, func), right: map(right, func)}

      %Not{expression: not_expr} = expr ->
        %{expr | expression: map(not_expr, func)}

      %Ash.Query.Parent{} = this ->
        # you have to map over the internals of this yourself
        func.(this)

      %Ash.Query.Exists{} = expr ->
        # you have to map over the internals of exists yourself
        func.(expr)

      %{__operator__?: true, left: left, right: right} = op ->
        %{op | left: map(left, func), right: map(right, func)}

      %{__function__?: true, arguments: arguments} = function ->
        %{
          function
          | arguments:
              Enum.map(arguments, fn
                {key, arg} when is_atom(key) ->
                  {key, map(arg, func)}

                arg ->
                  map(arg, func)
              end)
        }

      other ->
        func.(other)
    end
  end

  def flat_map(%__MODULE__{expression: nil}, _) do
    []
  end

  def flat_map(%__MODULE__{expression: expression}, func) do
    flat_map(expression, func)
  end

  def flat_map(expression, func) do
    do_flat_map(expression, func)
  end

  defp do_flat_map(expression, func) do
    case expression do
      %BooleanExpression{left: left, right: right} ->
        func.(expression) ++ flat_map(left, func) ++ flat_map(right, func)

      %Not{expression: not_expr} ->
        func.(expression) ++ flat_map(not_expr, func)

      %Ash.Query.Parent{} = this ->
        # you have to flat_map over the internals of this yourself
        func.(this)

      %Ash.Query.Exists{} = expr ->
        # you have to flat_map over the internals of exists yourself
        func.(expr)

      %{__operator__?: true, left: left, right: right} = op ->
        func.(op) ++ flat_map(left, func) ++ flat_map(right, func)

      %{__function__?: true, arguments: arguments} = function ->
        func.(function) ++ Enum.flat_map(arguments, &flat_map(&1, func))

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
         %BooleanExpression{op: op, left: left, right: right},
         api,
         resource,
         data
       ) do
    left_result = do_run_other_data_layer_filters(left, api, resource, data)
    right_result = do_run_other_data_layer_filters(right, api, resource, data)

    case {left_result, right_result} do
      {{:ok, left}, {:ok, right}} ->
        {:ok, BooleanExpression.optimized_new(op, left, right)}

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

  defp do_run_other_data_layer_filters(%Not{expression: expression}, api, resource, data) do
    case do_run_other_data_layer_filters(expression, api, resource, data) do
      {:ok, expr} -> {:ok, Not.new(expr)}
      {:filter_requests, requests} -> {:filter_requests, requests}
      {:error, error} -> {:error, error}
    end
  end

  defp do_run_other_data_layer_filters(
         %Ash.Query.Exists{path: path, expr: expr, at_path: at_path} = exists,
         api,
         resource,
         {request_path, tenant, data}
       ) do
    case shortest_path_to_changed_data_layer(resource, at_path ++ path) do
      {:ok, shortest_path} ->
        request_path = request_path ++ [:other_data_layer_filter_exists, at_path] ++ path
        related = Ash.Resource.Info.related(resource, shortest_path)

        case get_in(data, request_path ++ [:data]) do
          %{data: data} ->
            pkey = Ash.Resource.Info.primary_key(related)

            expr =
              Enum.reduce(data, nil, fn item, expr ->
                new_expr =
                  Enum.reduce(pkey, nil, fn key, expr ->
                    {:ok, new_expr} =
                      Ash.Query.Operator.new(
                        Ash.Query.Operator.Eq,
                        %Ash.Query.Ref{
                          attribute: key,
                          relationship_path: at_path
                        },
                        Map.get(item, key)
                      )

                    if expr do
                      Ash.Query.BooleanExpression.new(:and, expr, new_expr)
                    else
                      new_expr
                    end
                  end)

                if expr do
                  Ash.Query.BooleanExpression.new(:or, expr, new_expr)
                else
                  new_expr
                end
              end)

            {:ok, expr}

          nil ->
            {context, action} = last_relationship_context_and_action(resource, at_path ++ path)

            query =
              related
              |> Ash.Query.do_filter(expr)
              |> Ash.Query.set_context(context)

            {:filter_requests,
             Ash.Actions.Read.as_requests(
               request_path,
               query.resource,
               api,
               action,
               query: query,
               page: false,
               tenant: tenant
             )
             |> Enum.map(fn request ->
               # By returning the request and a key, we register a dependency on that key
               {request, :data}
             end)}
        end

      :error ->
        case do_run_other_data_layer_filters(
               expr,
               api,
               Ash.Resource.Info.related(resource, at_path ++ path),
               data
             ) do
          {:ok, new_nested} ->
            {:ok, %{exists | expr: new_nested}}

          {:error, error} ->
            {:error, error}

          {:filter_requests, requests} ->
            {:filter_requests, requests}
        end
    end
  end

  defp do_run_other_data_layer_filters(%{__predicate__?: _} = predicate, api, resource, data) do
    predicate
    |> relationship_paths()
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

  defp last_relationship_context_and_action(resource, [name]) do
    relationship = Ash.Resource.Info.relationship(resource, name)

    {relationship.context,
     relationship.read_action ||
       Ash.Resource.Info.primary_action!(relationship.destination, :read)}
  end

  defp last_relationship_context_and_action(resource, path) do
    second_to_last = Ash.Resource.Info.related(resource, :lists.droplast(path))

    relationship = Ash.Resource.Info.relationship(second_to_last, List.last(path))

    {relationship.context, relationship.read_action}
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

  defp scope_refs(%{__predicate__?: _, arguments: arguments} = pred, path) do
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
         {_, _, context} = data
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
      |> Ash.Query.new(ShadowApi)
      |> Ash.Query.do_filter(filter)
      |> Ash.Actions.Read.unpaginated_read(
        authorize?: context[:authorize?],
        actor: context[:actor]
      )
      |> case do
        {:ok, results} ->
          relationship.through
          |> Ash.Query.new(api)
          |> Ash.Query.do_filter([
            {relationship.destination_attribute_on_join_resource,
             in: Enum.map(results, &Map.get(&1, relationship.destination_attribute))}
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
    |> Ash.Query.sort(relationship.sort, prepend?: true)
    |> Ash.Query.set_context(relationship.context)
    |> filter_related_in(relationship, :lists.droplast(path), api, data)
  end

  defp filter_related_in(
         query,
         relationship,
         path,
         api,
         {request_path, tenant, data}
       ) do
    query = Ash.Query.set_tenant(query, tenant)
    request_path = request_path ++ [:other_data_layer_filter, path ++ [relationship.name], query]

    case get_in(data, request_path ++ [:data]) do
      %{data: records} ->
        records_to_expression(
          records,
          relationship,
          path
        )

      _ ->
        action =
          Ash.Resource.Info.action(relationship.destination, relationship.read_action) ||
            Ash.Resource.Info.primary_action!(relationship.destination, :read)

        action = %{action | pagination: false}

        {:filter_requests,
         Ash.Actions.Read.as_requests(request_path, query.resource, api, action,
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
        attribute: Ash.Resource.Info.attribute(relationship.source, relationship.source_attribute)
      },
      Map.get(single_record, relationship.destination_attribute)
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

  def relationship_paths(filter_or_expression, include_exists? \\ false, with_reference? \\ false)
  def relationship_paths(nil, _, _), do: []
  def relationship_paths(%{expression: nil}, _, _), do: []

  def relationship_paths(%__MODULE__{expression: expression}, include_exists?, with_reference?),
    do: relationship_paths(expression, include_exists?, with_reference?)

  def relationship_paths(expression, include_exists?, with_reference?) do
    paths =
      expression
      |> do_relationship_paths(include_exists?, with_reference?)
      |> List.wrap()
      |> List.flatten()

    if with_reference? do
      paths
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Map.new(fn {key, values} ->
        {key, Enum.uniq(values)}
      end)
    else
      paths
      |> Enum.uniq()
      |> Enum.map(fn {path} -> path end)
    end
  end

  defp do_relationship_paths(%Ref{relationship_path: path} = ref, _, true) do
    [{path, ref}]
  end

  defp do_relationship_paths(%Ref{relationship_path: path}, _, false) do
    [{path}]
  end

  defp do_relationship_paths(
         %BooleanExpression{left: left, right: right},
         include_exists?,
         with_reference?
       ) do
    [
      do_relationship_paths(left, include_exists?, with_reference?),
      do_relationship_paths(right, include_exists?, with_reference?)
    ]
  end

  defp do_relationship_paths(%Not{expression: expression}, include_exists?, with_reference?) do
    do_relationship_paths(expression, include_exists?, with_reference?)
  end

  defp do_relationship_paths(%Ash.Query.Exists{at_path: at_path}, false, with_reference?) do
    if with_reference? do
      [{at_path, nil}]
    else
      [{at_path}]
    end
  end

  defp do_relationship_paths(
         %Ash.Query.Exists{path: path, expr: expression, at_path: at_path},
         include_exists?,
         false
       ) do
    expression
    |> do_relationship_paths(include_exists?, false)
    |> List.flatten()
    |> Enum.flat_map(fn {rel_path} ->
      [{at_path}, {at_path ++ path ++ rel_path}]
    end)
    |> Kernel.++(parent_relationship_paths(expression, at_path, include_exists?, false))
  end

  defp do_relationship_paths(
         %Ash.Query.Exists{path: path, expr: expression, at_path: at_path},
         include_exists?,
         true
       ) do
    expression
    |> do_relationship_paths(include_exists?, true)
    |> List.flatten()
    |> Enum.flat_map(fn {rel_path, ref} ->
      [{at_path, nil}, {at_path ++ path ++ rel_path, ref}]
    end)
    |> Kernel.++(parent_relationship_paths(expression, at_path, include_exists?, true))
  end

  defp do_relationship_paths(
         %{__operator__?: true, left: left, right: right},
         include_exists?,
         with_reference?
       ) do
    Enum.flat_map([left, right], &do_relationship_paths(&1, include_exists?, with_reference?))
  end

  defp do_relationship_paths({key, value}, include_exists?, with_reference?) when is_atom(key) do
    do_relationship_paths(value, include_exists?, with_reference?)
  end

  defp do_relationship_paths(
         %{__function__?: true, arguments: arguments},
         include_exists?,
         with_reference?
       ) do
    Enum.flat_map(arguments, &do_relationship_paths(&1, include_exists?, with_reference?))
  end

  defp do_relationship_paths(value, include_exists?, with_reference?) when is_list(value) do
    Enum.flat_map(value, &do_relationship_paths(&1, include_exists?, with_reference?))
  end

  defp do_relationship_paths(_, _, _), do: []

  defp parent_relationship_paths(expression, at_path, include_exists?, with_reference?) do
    expression
    |> flat_map(fn
      %Ash.Query.Parent{expr: expr} ->
        expr
        |> do_relationship_paths(include_exists?, with_reference?)
        |> Enum.flat_map(fn
          {rel_path, ref} ->
            [{at_path ++ rel_path, ref}]

          {rel_path} ->
            [{at_path ++ rel_path}]
        end)

      _ ->
        []
    end)
  end

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

  def list_refs(expression, no_longer_simple? \\ false, in_an_eq? \\ false) do
    expression
    |> do_list_refs(no_longer_simple?, in_an_eq?)
    |> Enum.uniq()
  end

  defp do_list_refs(list, no_longer_simple?, in_an_eq? \\ false)

  defp do_list_refs(list, no_longer_simple?, in_an_eq?) when is_list(list) do
    Enum.flat_map(list, &do_list_refs(&1, no_longer_simple?, in_an_eq?))
  end

  defp do_list_refs({key, value}, no_longer_simple?, in_an_eq?) when is_atom(key),
    do: do_list_refs(value, no_longer_simple?, in_an_eq?)

  defp do_list_refs(%__MODULE__{expression: expression}, no_longer_simple?, in_an_eq?) do
    do_list_refs(expression, no_longer_simple?, in_an_eq?)
  end

  defp do_list_refs(expression, no_longer_simple?, in_an_eq?) do
    case expression do
      %BooleanExpression{left: left, right: right, op: op} ->
        no_longer_simple? = no_longer_simple? || op == :or
        do_list_refs(left, no_longer_simple?) ++ do_list_refs(right, no_longer_simple?)

      %Not{expression: not_expr} ->
        do_list_refs(not_expr, true)

      %struct{__predicate__?: _, left: left, right: right} ->
        in_an_eq? = struct == Ash.Query.Operator.Eq

        do_list_refs(left, no_longer_simple?, in_an_eq?) ++
          do_list_refs(right, no_longer_simple?, in_an_eq?)

      %{__predicate__?: _, arguments: args} ->
        Enum.flat_map(args, &do_list_refs(&1, true))

      value when is_list(value) ->
        Enum.flat_map(value, &do_list_refs(&1, true))

      %Ash.Query.Exists{at_path: at_path, path: path, expr: expr} ->
        parent_refs_inside_of_exists =
          flat_map(expr, fn
            %Ash.Query.Parent{expr: expr} ->
              expr
              |> do_list_refs(true)
              |> Enum.map(&%{&1 | relationship_path: at_path ++ &1.relationship_path})

            _ ->
              []
          end)

        expr
        |> do_list_refs(true)
        |> Enum.map(&%{&1 | relationship_path: at_path ++ path ++ &1.relationship_path})
        |> Enum.concat(parent_refs_inside_of_exists)

      %Call{args: args, relationship_path: relationship_path} ->
        args
        |> Enum.flat_map(&do_list_refs(&1, true))
        |> Enum.map(&%{&1 | relationship_path: relationship_path ++ &1.relationship_path})

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

      %Ash.Query.Exists{} = exists ->
        exists

      _ ->
        []
    end
  end

  defp attribute(%{public?: true, resource: resource}, attribute) when not is_nil(resource),
    do: Ash.Resource.Info.public_attribute(resource, attribute)

  defp attribute(%{public?: false, resource: resource}, attribute) when not is_nil(resource) do
    Ash.Resource.Info.attribute(resource, attribute)
  end

  defp attribute(_, _), do: nil

  defp aggregate(%{public?: true, resource: resource}, aggregate) when not is_nil(resource),
    do: Ash.Resource.Info.public_aggregate(resource, aggregate)

  defp aggregate(%{public?: false, resource: resource}, aggregate) when not is_nil(resource),
    do: Ash.Resource.Info.aggregate(resource, aggregate)

  defp aggregate(_, _), do: nil

  defp calculation(%{public?: true, resource: resource}, calculation) when not is_nil(resource),
    do: Ash.Resource.Info.public_calculation(resource, calculation)

  defp calculation(%{public?: false, resource: resource}, calculation) when not is_nil(resource),
    do: Ash.Resource.Info.calculation(resource, calculation)

  defp calculation(_, _), do: nil

  defp relationship(%{public?: true, resource: resource}, relationship)
       when not is_nil(resource) do
    Ash.Resource.Info.public_relationship(resource, relationship)
  end

  defp relationship(%{public?: false, resource: resource}, relationship)
       when not is_nil(resource) do
    Ash.Resource.Info.relationship(resource, relationship)
  end

  defp relationship(_, _), do: nil

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
    do: {:ok, move_to_relationship_path(expression, context[:relationship_path] || [])}

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
       move_to_relationship_path(adding_expression, context[:relationship_path] || [])
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

  defp add_expression_part(%Ash.Query.Parent{expr: expr} = this, context, expression) do
    case parse_expression(expr, %{context | resource: context.root_resource}) do
      {:ok, result} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, %{this | expr: result})}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(
         %Ash.Query.Exists{at_path: at_path, path: path, expr: exists_expression} = exists,
         context,
         expression
       ) do
    related = related(context, at_path ++ path)

    if !related do
      raise """
      Could not determine related resource for `exists/2` expression.

      Context Resource: #{inspect(context)}
      Context Relationship Path: #{inspect(context[:relationship_path])}
      At Path: #{inspect(at_path)}
      Path: #{inspect(path)}
      Related: #{inspect(related)}
      Expression: #{inspect(exists)}
      """
    end

    case parse_expression(
           exists_expression,
           %{
             context
             | resource: related,
               root_resource: related
           }
           |> Map.update(
             :parent_stack,
             [context[:root_resource]],
             &[context[:root_resource] | &1]
           )
         ) do
      {:ok, result} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, %{exists | expr: result})}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(%Ref{} = ref, _context, _expression) do
    {:ok, %{ref | bare?: true}}
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
          root_resource: context.root_resource,
          aggregates: context.aggregates,
          calculations: context.calculations,
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

  defp add_expression_part({:is_nil, attribute}, context, expression)
       when is_atom(attribute) or is_binary(attribute) do
    add_expression_part({attribute, [is_nil: true]}, context, expression)
  end

  defp add_expression_part({:fragment, _}, _context, _expression) do
    raise "Cannot use fragment outside of expression syntax"
  end

  defp add_expression_part({function, args}, context, expression)
       when (is_tuple(args) and is_atom(function)) or is_binary(function) do
    case get_function(function, context.resource, context.public?) do
      nil ->
        case calculation(context, function) do
          nil ->
            add_expression_part({function, [args]}, context, expression)

          resource_calculation ->
            {module, opts} = resource_calculation.calculation

            {args, nested_statement} =
              case args do
                {input, nested} ->
                  {input || %{}, nested}

                args ->
                  {args, []}
              end

            with {:ok, args} <-
                   Ash.Query.validate_calculation_arguments(resource_calculation, args),
                 {:ok, calculation} <-
                   Calculation.new(
                     resource_calculation.name,
                     module,
                     opts,
                     {resource_calculation.type, resource_calculation.constraints},
                     args,
                     resource_calculation.filterable?,
                     resource_calculation.load
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
        end

      function_module ->
        nested_statement = Tuple.to_list(args)

        with {:ok, args} <-
               hydrate_refs(List.wrap(nested_statement), context),
             refs <- list_refs(args),
             :ok <-
               validate_refs(
                 refs,
                 context.root_resource,
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
            if is_nil(context.resource) ||
                 Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
              {:ok, BooleanExpression.optimized_new(:and, expression, function)}
            else
              case function_module.evaluate(function) do
                {:known, result} ->
                  {:ok, result}

                _ ->
                  {:error, "data layer does not support the function #{inspect(function)}"}
              end
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

        read_action =
          aggregate.read_action || Ash.Resource.Info.primary_action!(related, :read).name

        with %{valid?: true} = aggregate_query <- Ash.Query.for_read(related, read_action),
             %{valid?: true} = aggregate_query <-
               Ash.Query.build(aggregate_query, filter: aggregate.filter, sort: aggregate.sort),
             {:ok, query_aggregate} <-
               Aggregate.new(
                 context.resource,
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
                 read_action:
                   aggregate.read_action ||
                     Ash.Resource.Info.primary_action!(
                       Ash.Resource.Info.related(context.resource, aggregate.relationship_path),
                       :read
                     ).name,
                 authorize?: aggregate.authorize?
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
        module =
          case Map.get(context.calculations, field) do
            %{calculation: {module, _}} ->
              module

            %{module: module} ->
              module
          end

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
        {module, opts} = resource_calculation.calculation

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
                 {resource_calculation.type, resource_calculation.constraints},
                 args,
                 resource_calculation.filterable?,
                 resource_calculation.load
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
               validate_refs(
                 refs,
                 context.root_resource,
                 {field, nested_statement}
               ),
             {:ok, operator} <- Operator.new(op_module, left, right) do
          if is_boolean(operator) do
            {:ok, BooleanExpression.optimized_new(:and, expression, operator)}
          else
            if is_nil(context.resource) ||
                 Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
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

  defp each_related(_resource, []), do: []

  defp each_related(resource, [item | rest]) do
    relationship = Ash.Resource.Info.relationship(resource, item)

    if relationship do
      if relationship.type == :many_to_many do
        [
          relationship.through,
          relationship.destination | each_related(relationship.destination, rest)
        ]
      else
        [relationship.destination | each_related(relationship.destination, rest)]
      end
    else
      {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

      Logger.warn(
        "Failed to detect relationship #{inspect(resource)} | #{inspect([item | rest])}\n#{Exception.format_stacktrace(stacktrace)}"
      )

      []
    end
  end

  defp validate_refs(refs, resource, expr) do
    with :ok <- validate_filterable_relationship_paths(refs, resource) do
      validate_not_crossing_data_layer_boundaries(refs, resource, expr)
    end
  end

  defp validate_filterable_relationship_paths(refs, resource) do
    Enum.find_value(
      refs,
      :ok,
      fn ref ->
        case check_filterable(resource, ref.relationship_path) do
          :ok ->
            false

          {:error, error} ->
            {:error, error}
        end
      end
    )
  end

  defp check_filterable(_resource, []), do: :ok

  defp check_filterable(resource, [relationship | rest]) do
    relationship = Ash.Resource.Info.relationship(resource, relationship)

    if relationship.filterable? do
      if Ash.DataLayer.data_layer_can?(resource, {:filter_relationship, relationship}) do
        check_filterable(relationship.destination, rest)
      else
        {:error, "#{inspect(resource)}.#{relationship.name} is not filterable"}
      end
    else
      {:error,
       "#{inspect(resource)}.#{relationship.name} has been configured as filterable?: false"}
    end
  end

  defp validate_not_crossing_data_layer_boundaries(refs, resource, expr) do
    refs
    |> Enum.flat_map(&each_related(resource, &1.relationship_path))
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

  defp resolve_call(
         %Call{name: name, args: args, operator?: true, relationship_path: relationship_path} =
           call,
         context
       ) do
    with :ok <- validate_datalayer_supports_nested_expressions(args, context.resource),
         {:op, op_module} when not is_nil(op_module) <-
           {:op, get_operator(name)},
         context <-
           Map.merge(context, %{
             resource: Ash.Resource.Info.related(context.resource, relationship_path),
             relationship_path: []
           }),
         {:ok, [left, right]} <-
           hydrate_refs(args, context),
         refs <- list_refs([left, right]),
         :ok <-
           validate_refs(refs, context.root_resource, call),
         {:ok, operator} <- Operator.new(op_module, left, right) do
      if is_boolean(operator) do
        {:ok, operator}
      else
        if is_nil(context.resource) ||
             Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
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

  defp resolve_call(%Call{name: :exists, args: [path_arg, expr_arg]} = call, context) do
    with {:ok, path} <- refs_to_path(path_arg),
         context <-
           %{
             context
             | resource:
                 Ash.Resource.Info.related(
                   context.resource,
                   call.relationship_path ++ path
                 ),
               relationship_path: []
           }
           |> Map.update(
             :parent_stack,
             [context[:root_resource]],
             &[context[:root_resource] | &1]
           ),
         {:ok, expr} <- hydrate_refs(expr_arg, context),
         refs <- list_refs(expr),
         :ok <-
           validate_refs(refs, context.resource, call) do
      {:ok, Ash.Query.Exists.new(path, expr, call.relationship_path)}
    else
      {:args, _} ->
        {:error, NoSuchFunction.exception(name: :exists, resource: context.resource)}

      other ->
        other
    end
  end

  defp resolve_call(%Call{name: name, args: args} = call, context) do
    could_be_calculation? = Enum.count(args) == 1 && Keyword.keyword?(Enum.at(args, 0))

    resource = Ash.Resource.Info.related(context.resource, call.relationship_path)

    context =
      Map.merge(context, %{
        resource: Ash.Resource.Info.related(context.resource, call.relationship_path),
        relationship_path: []
      })

    case {calculation(%{context | resource: resource}, name), could_be_calculation?} do
      {resource_calculation, true} when not is_nil(resource_calculation) ->
        {module, opts} = resource_calculation.calculation

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
                 {resource_calculation.type, resource_calculation.constraints},
                 args,
                 resource_calculation.filterable?,
                 resource_calculation.load
               ) do
          {:ok,
           %Ref{
             attribute: calculation,
             relationship_path:
               Map.get(context, :relationship_path, []) ++ call.relationship_path,
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
             :ok <- validate_refs(refs, context.root_resource, call),
             {:func, function_module} when not is_nil(function_module) <-
               {:func, get_function(name, context.resource, context.public?)},
             {:ok, function} <-
               Function.new(
                 function_module,
                 args
               ) do
          if is_boolean(function) do
            {:ok, function}
          else
            if is_nil(context.resource) ||
                 Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
              {:ok, function}
            else
              case function_module.evaluate(function) do
                {:known, result} ->
                  {:ok, result}

                _ ->
                  {:error, "data layer does not support the function #{inspect(function)}"}
              end
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

  defp refs_to_path(%Ref{relationship_path: relationship_path, attribute: attribute}) do
    attribute =
      case attribute do
        %{name: name} ->
          name

        name ->
          name
      end

    {:ok, relationship_path ++ [attribute]}
  end

  defp refs_to_path(value) do
    {:error, "#{inspect(value)} is not a valid path for exists/2"}
  end

  defp validate_datalayer_supports_nested_expressions(args, resource) do
    if resource && Enum.any?(args, &Ash.Filter.TemplateHelpers.expr?/1) &&
         !Ash.DataLayer.data_layer_can?(resource, :nested_expressions) do
      {:error, "Datalayer does not support nested expressions"}
    else
      :ok
    end
  end

  def hydrate_refs(value, context) do
    context =
      context
      |> Map.put_new(:resource, nil)
      |> Map.put_new(:root_resource, context[:resource])
      |> Map.put_new(:public?, false)

    do_hydrate_refs(value, context)
  end

  def do_hydrate_refs({:ref, value}, context) do
    do_hydrate_refs(
      %Ash.Query.Ref{
        attribute: value,
        relationship_path: [],
        resource: context.root_resource
      },
      context
    )
  end

  def do_hydrate_refs({key, value}, context) when is_atom(key) or is_binary(key) do
    case do_hydrate_refs(value, context) do
      {:ok, hydrated} ->
        {:ok, {key, hydrated}}

      other ->
        other
    end
  end

  def do_hydrate_refs(
        %Ref{attribute: attribute} = ref,
        %{aggregates: aggregates, calculations: calculations} = context
      )
      when is_atom(attribute) or is_binary(attribute) do
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
            {module, opts} = resource_calculation.calculation

            with {:ok, args} <-
                   Ash.Query.validate_calculation_arguments(resource_calculation, %{}),
                 {:ok, calculation} <-
                   Calculation.new(
                     resource_calculation.name,
                     module,
                     opts,
                     {resource_calculation.type, resource_calculation.constraints},
                     args,
                     resource_calculation.filterable?,
                     resource_calculation.load
                   ) do
              {:ok, %{ref | attribute: calculation, resource: related}}
            else
              {:error, error} ->
                {:error, error}
            end

          aggregate = aggregate(context, attribute) ->
            agg_related = Ash.Resource.Info.related(related, aggregate.relationship_path)

            read_action =
              aggregate.read_action || Ash.Resource.Info.primary_action!(agg_related, :read).name

            with %{valid?: true} = aggregate_query <-
                   Ash.Query.for_read(agg_related, read_action),
                 %{valid?: true} = aggregate_query <-
                   Ash.Query.build(aggregate_query, filter: aggregate.filter, sort: aggregate.sort),
                 {:ok, query_aggregate} <-
                   Aggregate.new(
                     related,
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
                    attribute: attribute(%{context | resource: relationship.destination}, key),
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

  def do_hydrate_refs(%Ref{relationship_path: relationship_path, resource: nil} = ref, context) do
    {:ok, %{ref | resource: Ash.Resource.Info.related(context.resource, relationship_path)}}
  end

  def do_hydrate_refs(%BooleanExpression{left: left, right: right} = expr, context) do
    with {:ok, left} <- do_hydrate_refs(left, context),
         {:ok, right} <- do_hydrate_refs(right, context) do
      {:ok, %{expr | left: left, right: right}}
    else
      other ->
        other
    end
  end

  def do_hydrate_refs(%Not{expression: expression} = expr, context) do
    with {:ok, expression} <- do_hydrate_refs(expression, context) do
      {:ok, %{expr | expression: expression}}
    end
  end

  def do_hydrate_refs(%Call{} = call, context) do
    resolve_call(call, context)
  end

  def do_hydrate_refs(%{__predicate__?: _, left: left, right: right} = expr, context) do
    with {:ok, left} <- do_hydrate_refs(left, context),
         {:ok, right} <- do_hydrate_refs(right, context) do
      {:ok, %{expr | left: left, right: right}}
    else
      other ->
        other
    end
  end

  def do_hydrate_refs(%{__predicate__?: _, arguments: arguments} = expr, context) do
    case do_hydrate_refs(arguments, context) do
      {:ok, args} ->
        {:ok, %{expr | arguments: args}}

      other ->
        other
    end
  end

  def do_hydrate_refs(%Ash.Query.Parent{expr: expr} = this, context) do
    context =
      %{
        context
        | resource: hd(context.parent_stack),
          root_resource: hd(context.parent_stack),
          parent_stack: tl(context.parent_stack)
      }
      |> Map.put(:relationship_path, [])

    case do_hydrate_refs(expr, context) do
      {:ok, expr} ->
        {:ok, %{this | expr: expr}}

      other ->
        other
    end
  end

  def do_hydrate_refs(
        %Ash.Query.Exists{expr: expr, at_path: at_path, path: path} = exists,
        context
      ) do
    new_resource = Ash.Resource.Info.related(context[:resource], at_path ++ path)

    context = %{
      resource: new_resource,
      root_resource: new_resource,
      parent_stack: [context[:root_resource] | context[:parent_stack] || []],
      relationship_path: [],
      aggregates: %{},
      calculations: %{},
      public?: context[:public?],
      data_layer: Ash.DataLayer.data_layer(new_resource)
    }

    case do_hydrate_refs(expr, context) do
      {:ok, expr} ->
        {:ok, %{exists | expr: expr}}

      other ->
        other
    end
  end

  def do_hydrate_refs(list, context) when is_list(list) do
    list
    |> Enum.reduce_while({:ok, []}, fn val, {:ok, acc} ->
      case do_hydrate_refs(val, context) do
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

  def do_hydrate_refs(val, _context) do
    {:ok, val}
  end

  defp add_aggregate_expression(context, nested_statement, field, expression) do
    if context.resource && Ash.DataLayer.data_layer_can?(context.resource, :aggregate_filter) do
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
    if context.resource &&
         Ash.DataLayer.data_layer_can?(context.resource, :expression_calculation) &&
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

  def move_exprs_to_relationship_path(refs, []), do: refs

  def move_exprs_to_relationship_path(refs, path) do
    Enum.map(refs, &move_to_relationship_path(&1, path))
  end

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

      %Ash.Query.Exists{expr: expr} = exists ->
        %{
          exists
          | at_path: relationship_path ++ exists.at_path,
            expr:
              map(expr, fn
                %Ash.Query.Parent{} = this ->
                  move_to_relationship_path(this, relationship_path)

                other ->
                  other
              end)
        }

      %Ash.Query.Parent{expr: expr} = this ->
        %{this | expr: move_to_relationship_path(expr, relationship_path)}

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

  defp parse_predicates(%struct{} = value, field, context)
       when struct not in [Not, BooleanExpression, Ref, Call] do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(values, attr, context) do
    if is_struct(values) && Map.has_key?(values, :__predicate__) do
      parse_predicates([eq: values], attr, context)
    else
      if is_map(values) || Keyword.keyword?(values) do
        Enum.reduce_while(values, {:ok, nil}, fn
          {:not, value}, {:ok, expression} ->
            case parse_predicates(List.wrap(value), attr, context) do
              {:ok, not_expression} ->
                {:cont,
                 {:ok,
                  BooleanExpression.optimized_new(:and, expression, %Not{
                    expression: not_expression
                  })}}

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
                  relationship_path: context[:relationship_path] || [],
                  resource: context.resource
                }

                with {:ok, [left, right]} <-
                       hydrate_refs([left, value], context),
                     refs <- list_refs([left, right]),
                     :ok <-
                       validate_refs(
                         refs,
                         context.root_resource,
                         {attr, value}
                       ),
                     {:ok, operator} <- Operator.new(operator_module, left, right) do
                  if is_boolean(operator) do
                    {:cont, {:ok, operator}}
                  else
                    if is_nil(context.resource) ||
                         Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, operator}) do
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
  end

  def get_function(key, resource, public?) when is_atom(key) do
    function = @builtin_functions[key]

    function =
      if function do
        function
      else
        if resource do
          Enum.find(Ash.DataLayer.data_layer_functions(resource), &(&1.name() == key))
        end
      end

    if public? && function && function.private?() do
      nil
    else
      function
    end
  end

  def get_function(key, resource, public?) when is_binary(key) do
    function =
      Map.get(@string_builtin_functions, key) ||
        Enum.find(Ash.DataLayer.data_layer_functions(resource), &(&1.name() == key))

    if public? && function && function.private?() do
      nil
    else
      function
    end
  end

  def get_function(_, _, _), do: nil

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
      expression = sanitize(expression)
      concat(["#Ash.Filter<", to_doc(expression, opts), ">"])
    end

    defp sanitize(%BooleanExpression{left: left, right: right} = expr) do
      %{expr | left: sanitize(left), right: sanitize(right)}
    end

    defp sanitize(%Not{expression: expression} = not_expr) do
      %{not_expr | expression: sanitize(expression)}
    end

    defp sanitize(%{__operator__?: true, left: left, right: right} = op) do
      [left, right] = poison_exprs([left, right])
      %{op | left: left, right: right}
    end

    defp sanitize(%{__function__?: true, arguments: arguments} = func) do
      %{func | arguments: poison_exprs(arguments)}
    end

    defp sanitize(%Call{args: arguments} = call) do
      %{call | args: poison_exprs(arguments)}
    end

    defp sanitize(%Ash.Query.Parent{expr: expr} = exists) do
      %{exists | expr: sanitize(expr)}
    end

    defp sanitize(%Ash.Query.Exists{expr: expr} = exists) do
      %{exists | expr: sanitize(expr)}
    end

    defp sanitize(other) do
      other
    end

    defp poison_exprs(values) do
      if Enum.any?(values, &refers_to_sensitive?/1) do
        Enum.map(values, &scrub_values/1)
      else
        values
      end
    end

    defp scrub_values(%BooleanExpression{left: left, right: right} = expr) do
      %{expr | left: scrub_values(left), right: scrub_values(right)}
    end

    defp scrub_values(%Not{expression: expression} = not_expr) do
      %{not_expr | expression: scrub_values(expression)}
    end

    defp scrub_values(%{__operator__?: true, left: left, right: right} = op) do
      [left, right] = poison_exprs([left, right])
      %{op | left: left, right: right}
    end

    defp scrub_values(%{__function__?: true, arguments: arguments} = func) do
      %{func | arguments: poison_exprs(arguments)}
    end

    defp scrub_values(%Call{args: arguments} = call) do
      %{call | args: poison_exprs(arguments)}
    end

    defp scrub_values(%Ref{} = ref), do: ref

    defp scrub_values(_other) do
      "**redacted**"
    end

    defp refers_to_sensitive?(%BooleanExpression{left: left, right: right}) do
      Enum.any?([left, right], &refers_to_sensitive?/1)
    end

    defp refers_to_sensitive?(%Not{expression: expression}) do
      refers_to_sensitive?(expression)
    end

    defp refers_to_sensitive?(%{__operator__?: true, left: left, right: right}) do
      Enum.any?([left, right], &refers_to_sensitive?/1)
    end

    defp refers_to_sensitive?(%{__function__?: true, arguments: arguments}) do
      Enum.any?(arguments, &refers_to_sensitive?/1)
    end

    defp refers_to_sensitive?(%Call{args: arguments}) do
      Enum.any?(arguments, &refers_to_sensitive?/1)
    end

    defp refers_to_sensitive?(%Ref{attribute: %{sensitive?: true}}), do: true

    defp refers_to_sensitive?(_other) do
      false
    end
  end
end
