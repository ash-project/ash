defmodule Ash.Filter do
  # credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
  @dialyzer {:nowarn_function, do_map: 2, map: 2}
  require Logger
  require Ash.Expr

  alias Ash.Error.Query.{
    InvalidFilterValue,
    NoSuchField,
    NoSuchFilterPredicate,
    NoSuchFunction,
    NoSuchOperator
  }

  alias Ash.Error.Invalid.InvalidPrimaryKey

  alias Ash.Query.Function.{
    Ago,
    At,
    CompositeType,
    Contains,
    CountNils,
    DateAdd,
    DateTimeAdd,
    Error,
    Fragment,
    FromNow,
    GetPath,
    If,
    IsNil,
    Lazy,
    Length,
    Minus,
    Now,
    Round,
    StringDowncase,
    StringJoin,
    StringLength,
    StringSplit,
    StringTrim,
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

  @custom_expressions Application.compile_env(:ash, :custom_expressions) || []

  @functions [
    Ago,
    At,
    CompositeType,
    Contains,
    CountNils,
    DateAdd,
    DateTimeAdd,
    Fragment,
    FromNow,
    GetPath,
    IsNil,
    If,
    Lazy,
    Length,
    Minus,
    Now,
    Error,
    Round,
    Today,
    Type,
    StringDowncase,
    StringJoin,
    StringLength,
    StringSplit,
    StringTrim
  ]

  @inline_aggregates [:count, :first, :sum, :list, :max, :min, :avg, :custom_aggregate]

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

  ## Security Concerns

  If you are using a map with string keys, it is likely that you are parsing
  input. It is important to note that, instead of passing a filter supplied from
  an external source directly to `Ash.Query.filter/2`, you should call
  `Ash.Filter.parse_input/2`.  This ensures that the filter only uses public
  attributes, relationships, aggregates and calculations, honors field policies
  and any policies on related resources.

  ## Writing a filter

  ### Built In Predicates

  #{Enum.map_join(@operators, "\n", &"* `#{&1.operator()}`")}
  #{Enum.map_join(@operator_aliases, "\n", fn {key, val} -> "* `#{key}` (alias
  for `#{val.operator()}`)" end)}

  ### BooleanExpression syntax

  The expression syntax ultimately just builds the keyword list style filter,
  but with lots of conveniences that would be very annoying to do manually.

  Examples

  ```elixir
  Ash.Query.filter(resource, name == "Zardoz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name == "Doz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name in ["Doz", "Daz"] and high_score > 10)
  Ash.Query.filter(resource, first_name == "Zar" or last_name == "Doz" or (high_score > 10 and high_score < -10))
  ```

  ### Expressions

  More complex filters can be built using Ash Expressions.

  Examples

  ```elixir
  # Filter based on the contents of a string attribute
  Ash.Query.filter(Helpdesk.Support.Ticket, contains(subject, "2"))
  # Filter based on the attribute of a joined relationship:
  Ash.Query.filter(Helpdesk.Support.Ticket, representative.name == ^name)
  ```

  See the [Expressions guide](/documentation/topics/reference/expressions.md)
  guide for more information.

  ### Keyword list syntax

  A filter is a nested keyword list (with some exceptions, like `true` for
  everything and `false` for nothing).

  The key is the "predicate" (or "condition") and the value is the parameter.
  You can use `and` and `or` to create nested filters. Data layers can expose
  custom predicates. Eventually, you will be able to define your own custom
  predicates, which will be a mechanism for you to attach complex filters
  supported by the data layer to your queries.

  ** Important ** In a given keyword list, all predicates are considered to be
  "ands". So `[or: [first_name: "Tom", last_name: "Bombadil"]]` doesn't mean
  'First name == "tom" or last_name == "bombadil"'. To say that, you want to
  provide a list of filters, like so: `[or: [[first_name: "Tom"], [last_name:
  "Bombadil"]]]`

  Some example filters:

  ```elixir
  Ash.Query.filter(resource, [name: "Zardoz"])
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

  Maps are also accepted, as are maps with string keys. Technically, a list of
  `[{"string_key", value}]` would also work.
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
  defmodule ShadowDomain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered?(true)
    end
  end

  @doc """
  Parses a filter statement, accepting only public attributes/relationships,
  honoring field policies & related resource policies.

  See `parse/2` for more
  """
  def parse_input(
        resource,
        statement
      ) do
    context = %{
      resource: resource,
      root_resource: resource,
      relationship_path: [],
      public?: true,
      input?: true,
      data_layer: Ash.DataLayer.data_layer(resource)
    }

    with {:ok, expression} <- parse_expression(statement, context),
         {:ok, expression} <- hydrate_refs(expression, context),
         :ok <- validate_references(expression, resource) do
      {:ok, %__MODULE__{expression: expression, resource: resource}}
    end
  end

  @doc """
  Parses a filter statement, accepting only public attributes/relationships,
  honoring field policies & related resource policies, raising on errors.

  See `parse_input/2` for more
  """
  def parse_input!(resource, statement) do
    case parse_input(resource, statement) do
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
  def parse!(resource, statement, context \\ %{}) do
    case parse(resource, statement, context) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise Ash.Error.to_error_class(error,
                bread_crumbs: parse_bread_crumbs(resource, statement, context)
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
  """
  def parse(resource, statement, context \\ %{})

  def parse(_resource, nil, _context) do
    {:ok, nil}
  end

  def parse(resource, statement, original_context) do
    context =
      Map.merge(
        %{
          resource: resource,
          relationship_path: [],
          public?: false,
          input?: false,
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
            NoSuchField.exception(
              field: attribute,
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

  defp get_keys(value, fields, resource, nils_distinct? \\ true) do
    original_value = value

    Enum.reduce_while(fields, {:ok, %{}}, fn field, {:ok, vals} ->
      case fetch(value, field) do
        {:ok, value} ->
          case cast_value(resource, field, value, original_value) do
            {:ok, value} ->
              if value == nil && nils_distinct? do
                {:halt, :error}
              else
                {:cont, {:ok, Map.put(vals, field, value)}}
              end

            {:error, _error} ->
              {:halt, :error}
          end

        :error ->
          case fetch(value, to_string(field)) do
            {:ok, value} ->
              case cast_value(resource, field, value, original_value) do
                {:ok, value} ->
                  if value == nil && nils_distinct? do
                    {:halt, :error}
                  else
                    {:cont, {:ok, Map.put(vals, field, value)}}
                  end

                {:error, _error} ->
                  {:halt, :error}
              end

            :error ->
              {:halt, :error}
          end
      end
    end)
    |> case do
      {:ok, values} ->
        {:ok,
         Map.new(values, fn
           {key, nil} ->
             {key, [is_nil: true]}

           {key, value} ->
             {key, value}
         end)}

      :error ->
        :error

      {:error, error} ->
        {:error, error}
    end
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
        case get_keys(id, identity.keys, resource, identity.nils_distinct?) do
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
    opts = Spark.Options.validate!(opts, @to_simple_filter_options)
    predicates = get_predicates(expression, opts[:skip_invalid?])

    %Simple{resource: resource, predicates: predicates}
  end

  @doc """
  Can be used to find a simple equality predicate on an attribute

  Use this when your attribute is configured with `filterable? :simple_equality`, and you want to
  to find the value that it is being filtered on with (if any).
  """
  def find_simple_equality_predicate(expression, attribute) do
    expression
    |> find(&simple_eq?(&1, attribute), false)
    |> case do
      nil ->
        nil

      %{right: right, left: left} ->
        Enum.find([right, left], fn value ->
          !Ash.Expr.expr?(value)
        end)
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
    do_find(expr, pred, true, true, true)
  end

  @doc "Find an expression inside of a filter that matches the provided predicate"
  def find(expr, pred, ors? \\ true, ands? \\ true) do
    do_find(expr, pred, false, ors?, ands?)
  end

  defp do_find(expr, pred, value?, ors?, ands?) do
    if value = pred.(expr) do
      if value? do
        value
      else
        expr
      end
    else
      case expr do
        %__MODULE__{expression: expression} ->
          find(expression, pred, ors?, ands?)

        %Not{expression: expression} ->
          find(expression, pred, ors?, ands?)

        %BooleanExpression{op: op, left: left, right: right} ->
          cond do
            op == :or && !ors? ->
              nil

            op == :and && !ands? ->
              nil

            true ->
              find(left, pred, ors?, ands?) || find(right, pred, ors?, ands?)
          end

        %Call{args: arguments} ->
          Enum.find(arguments, &find(&1, pred, ors?, ands?))

        %{__operator__?: true, left: left, right: right} ->
          find(left, pred, ors?, ands?) || find(right, pred, ors?, ands?)

        %{__function__?: true, arguments: arguments} ->
          Enum.find(arguments, &find(&1, pred, ors?, ands?))

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
        _calculations \\ %{},
        _aggregates \\ %{},
        return_refs? \\ false
      ) do
    filter
    |> list_refs()
    |> Enum.filter(fn
      %{attribute: %Calculation{}} ->
        true

      _ ->
        false
    end)
    |> Enum.flat_map(fn %{attribute: calculation} = calculation_ref ->
      if calculation.module.has_expression? do
        expression = calculation.module.expression(calculation.opts, calculation.context)

        case hydrate_refs(expression, %{
               resource: Ash.Resource.Info.related(resource, calculation_ref.relationship_path),
               relationship_path: [],
               public?: false
             }) do
          {:ok, expression} ->
            [
              calculation_ref
              | used_calculations(
                  expression,
                  Ash.Resource.Info.related(resource, calculation_ref.relationship_path),
                  :*,
                  %{},
                  %{},
                  true
                )
            ]

          _ ->
            [calculation_ref]
        end
      else
        [calculation_ref]
      end
    end)
    |> Enum.filter(fn
      %Ref{attribute: %Calculation{}, relationship_path: ref_relationship_path} ->
        relationship_path == :* ||
          (relationship_path in [nil, []] and ref_relationship_path in [nil, []]) ||
          relationship_path == ref_relationship_path

      _ ->
        false
    end)
    |> then(fn refs ->
      if return_refs? do
        refs
      else
        Enum.map(refs, & &1.attribute)
      end
    end)
  end

  def used_aggregates(filter, relationship_path \\ [], return_refs? \\ false) do
    refs =
      filter
      |> list_refs(false, false, true)
      |> Enum.filter(fn
        %Ref{attribute: %Aggregate{}, relationship_path: ref_relationship_path} ->
          relationship_path == :* ||
            (relationship_path in [nil, []] and ref_relationship_path in [nil, []]) ||
            relationship_path == ref_relationship_path

        _ref ->
          false
      end)
      |> expand_aggregates()

    if return_refs? do
      refs
    else
      Enum.map(refs, & &1.attribute)
    end
    |> Enum.uniq()
  end

  defp expand_aggregates(aggregates) do
    aggregates
    |> Enum.flat_map(fn
      %{field: %Ash.Query.Aggregate{} = inner_aggregate} = aggregate ->
        [aggregate, inner_aggregate | expand_aggregates(aggregate)]

      other ->
        [other]
    end)
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
                bread_crumbs: parse_bread_crumbs(base.resource, addition, context)
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
    case parse(base.resource, statement, context) do
      {:ok, filter} -> add_to_filter(base, filter, op, aggregates, calculations)
      {:error, error} -> {:error, error}
    end
  end

  defp parse_bread_crumbs(%{resource: resource} = _filter, addition, context) do
    parse_bread_crumbs(resource, addition, context)
  end

  defp parse_bread_crumbs(resource, addition, context) do
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

  @doc false
  def relationship_filters(domain, query, actor, tenant, aggregates, authorize?, filters \\ %{}) do
    if authorize? do
      paths_with_refs =
        query.filter
        |> relationship_paths(true, true, true)
        |> Enum.map(fn {path, refs} ->
          refs = Enum.filter(refs, & &1.input?)
          {path, refs}
        end)
        |> Enum.reject(fn {path, refs} -> path == [] || refs == [] end)

      refs =
        group_refs_by_all_paths(paths_with_refs)

      paths_with_refs
      |> Enum.map(&elem(&1, 0))
      |> Enum.reduce_while({:ok, filters}, fn path, {:ok, filters} ->
        last_relationship = last_relationship(query.resource, path)

        add_authorization_path_filter(
          filters,
          last_relationship,
          domain,
          query,
          actor,
          tenant,
          refs
        )
      end)
      |> add_aggregate_path_authorization(
        domain,
        refs,
        aggregates,
        query,
        actor,
        tenant,
        refs,
        authorize?
      )
    else
      {:ok, filters}
    end
  end

  defp add_authorization_path_filter(
         filters,
         last_relationship,
         domain,
         _query,
         actor,
         tenant,
         _refs,
         base_related_query \\ nil,
         _aggregate? \\ false
       ) do
    case relationship_query(last_relationship, domain, actor, tenant, base_related_query) do
      %{errors: []} = related_query ->
        if filters[{last_relationship.source, last_relationship.name, related_query.action.name}] do
          {:cont, {:ok, filters}}
        else
          related_query
          |> Ash.Query.set_context(%{
            accessing_from: %{
              source: last_relationship.source,
              name: last_relationship.name
            }
          })
          |> Ash.Query.select([])
          |> Ash.can(actor,
            run_queries?: false,
            pre_flight?: false,
            alter_source?: true,
            no_check?: true,
            return_forbidden_error?: true,
            maybe_is: false
          )
          |> case do
            {:ok, true, authorized_related_query} ->
              {:cont,
               {:ok,
                Map.put(
                  filters,
                  {last_relationship.source, last_relationship.name, related_query.action.name},
                  authorized_related_query.filter
                )}}

            {:ok, false, _error} ->
              {:halt,
               {:ok,
                Map.put(
                  filters,
                  {last_relationship.source, last_relationship.name, related_query.action.name},
                  false
                )}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end

      %{errors: errors} ->
        {:halt, {:error, errors}}
    end
  end

  defp add_aggregate_path_authorization(
         {:ok, path_filters},
         domain,
         refs,
         aggregates,
         query,
         actor,
         tenant,
         refs,
         authorize?
       ) do
    refs
    |> Enum.flat_map(fn {_path, refs} ->
      refs
      |> Enum.filter(
        &match?(
          %Ref{attribute: %Ash.Query.Aggregate{}, input?: true},
          &1
        )
      )
      |> Enum.map(& &1.attribute)
    end)
    |> Enum.concat(aggregates)
    |> Enum.reduce_while({:ok, path_filters}, fn aggregate, {:ok, filters} ->
      aggregate.relationship_path
      |> :lists.droplast()
      |> Ash.Query.Aggregate.subpaths()
      |> Enum.reduce_while({:ok, filters}, fn subpath, {:ok, filters} ->
        last_relationship = last_relationship(query.resource, subpath)

        add_authorization_path_filter(
          filters,
          last_relationship,
          domain,
          query,
          actor,
          tenant,
          refs,
          Ash.Query.for_read(
            last_relationship.destination,
            Ash.Resource.Info.primary_action(last_relationship.destination, :read).name,
            actor: actor,
            tenant: tenant,
            authorize?: authorize?
          ),
          true
        )
      end)
      |> case do
        {:ok, filters} ->
          last_relationship = last_relationship(aggregate.resource, aggregate.relationship_path)

          case relationship_filters(
                 domain,
                 aggregate.query,
                 actor,
                 tenant,
                 [],
                 authorize?,
                 filters
               ) do
            {:ok, filters} ->
              add_authorization_path_filter(
                filters,
                last_relationship,
                domain,
                query,
                actor,
                tenant,
                refs,
                aggregate.query,
                true
              )

            {:error, error} ->
              {:error, error}
          end

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp relationship_query(relationship, domain, actor, tenant, base) do
    base_query = base || Ash.Query.new(relationship.destination)
    domain = relationship.domain || domain

    action =
      relationship.read_action || (base_query.action && base_query.action.name) ||
        Ash.Resource.Info.primary_action!(relationship.destination, :read).name

    query =
      relationship.destination
      |> Ash.Query.set_context(relationship.context)
      |> Ash.Query.sort(relationship.sort, prepend?: true)
      |> Ash.Query.do_filter(relationship.filter, parent_stack: [relationship.source])

    if query.__validated_for_action__ == action do
      query
    else
      Ash.Query.for_read(query, action, %{},
        actor: actor,
        authorize?: true,
        tenant: tenant,
        domain: domain
      )
    end
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

      value when is_tuple(value) ->
        value
        |> Tuple.to_list()
        |> map(func)
        |> List.to_tuple()

      value when is_list(value) ->
        Enum.map(value, &map(&1, func))

      value when is_map(value) ->
        value
        |> Map.to_list()
        |> map(func)
        |> Map.new()

      %BooleanExpression{left: left, right: right} = expr ->
        %{expr | left: map(left, func), right: map(right, func)}

      %Not{expression: not_expr} = expr ->
        %{expr | expression: map(not_expr, func)}

      %Ash.Query.Parent{} = this ->
        # you have to map over the internals of this yourself
        func.(this)

      %Ash.CustomExpression{expression: expression, simple_expression: simple_expression} =
          custom_expression ->
        %{
          custom_expression
          | expression: map(expression, func),
            simple_expression: map(simple_expression, func)
        }

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

      %Ash.CustomExpression{expression: expression, simple_expression: simple_expression} ->
        flat_map(expression, func) ++ flat_map(simple_expression, func)

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

  def update_aggregates(expression, mapper, nested_path \\ [], parent_paths \\ [])

  def update_aggregates(
        %__MODULE__{expression: expression} = filter,
        mapper,
        nested_path,
        parent_paths
      ) do
    %{filter | expression: update_aggregates(expression, mapper, nested_path, parent_paths)}
  end

  def update_aggregates(expression, mapper, nested_path, parent_paths) do
    case expression do
      {key, value} when is_atom(key) ->
        {key, update_aggregates(value, mapper, nested_path, parent_paths)}

      %Ash.Query.Exists{expr: expr, path: path, at_path: at_path} = exists ->
        %{
          exists
          | expr: update_aggregates(expr, mapper, at_path ++ path, [nested_path | parent_paths])
        }

      %Ash.Query.Parent{expr: expr} = exists ->
        %{
          exists
          | expr:
              update_aggregates(
                expr,
                mapper,
                Enum.at(parent_paths, 0) || [],
                Enum.drop(parent_paths, 1)
              )
        }

      %Not{expression: expression} = not_expr ->
        %{not_expr | expression: update_aggregates(expression, mapper, nested_path, parent_paths)}

      %BooleanExpression{left: left, right: right} = expression ->
        %{
          expression
          | left: update_aggregates(left, mapper, nested_path, parent_paths),
            right: update_aggregates(right, mapper, nested_path, parent_paths)
        }

      %{__operator__?: true, left: left, right: right} = op ->
        left = update_aggregates(left, mapper, nested_path)
        right = update_aggregates(right, mapper, nested_path)
        %{op | left: left, right: right}

      %{__function__?: true, arguments: args} = func ->
        %{
          func
          | arguments: Enum.map(args, &update_aggregates(&1, mapper, nested_path, parent_paths))
        }

      %Ref{attribute: %Aggregate{} = agg} = ref ->
        %{
          ref
          | attribute:
              mapper.(agg, %{ref | relationship_path: nested_path ++ ref.relationship_path})
        }

      other ->
        other
    end
  end

  def run_other_data_layer_filters(domain, resource, %{expression: expression} = filter, tenant) do
    case do_run_other_data_layer_filters(expression, domain, resource, tenant) do
      {:ok, new_expression} -> {:ok, %{filter | expression: new_expression}}
      {:error, error} -> {:error, error}
    end
  end

  def run_other_data_layer_filters(_, _, filter, _tenant) when filter in [nil, true, false],
    do: {:ok, filter}

  defp do_run_other_data_layer_filters(
         %BooleanExpression{op: op, left: left, right: right},
         domain,
         resource,
         tenant
       ) do
    left_result = do_run_other_data_layer_filters(left, domain, resource, tenant)
    right_result = do_run_other_data_layer_filters(right, domain, resource, tenant)

    case {left_result, right_result} do
      {{:ok, left}, {:ok, right}} ->
        {:ok, BooleanExpression.optimized_new(op, left, right)}

      {{:error, error}, _} ->
        {:error, error}

      {_, {:error, error}} ->
        {:error, error}
    end
  end

  defp do_run_other_data_layer_filters(%Not{expression: expression}, domain, resource, tenant) do
    case do_run_other_data_layer_filters(expression, domain, resource, tenant) do
      {:ok, expr} -> {:ok, Not.new(expr)}
      {:error, error} -> {:error, error}
    end
  end

  defp do_run_other_data_layer_filters(
         %Ash.Query.Exists{path: path, expr: expr, at_path: at_path} = exists,
         domain,
         resource,
         tenant
       ) do
    case shortest_path_to_changed_data_layer(resource, at_path ++ path) do
      {:ok, shortest_path} ->
        related = Ash.Resource.Info.related(resource, shortest_path)

        # We should do these asynchronously in parallel
        # We used to, but this was changed to happen synchronously as part
        # of an architecture simplification (removal of Ash.Engine)
        {relationship, context, _action} =
          last_relationship_context_and_action(resource, at_path ++ path)

        query =
          related
          |> Ash.Query.do_filter(expr)
          |> Ash.Query.set_context(context)
          |> Ash.Query.set_tenant(tenant)
          |> Map.put(:domain, domain)
          |> Ash.Query.set_context(%{private: %{internal?: true}})

        case Ash.Actions.Read.unpaginated_read(query, relationship.read_action) do
          {:ok, data} ->
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

          {:error, error} ->
            {:error, error}
        end

      :error ->
        {:ok, exists}
    end
  end

  defp do_run_other_data_layer_filters(%{__predicate__?: _} = predicate, domain, resource, tenant) do
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

        fetch_related_data(resource, path, new_predicate, domain, relationship, tenant)
    end
  end

  defp do_run_other_data_layer_filters(other, _domain, _resource, _data), do: {:ok, other}

  defp last_relationship_context_and_action(resource, [name]) do
    relationship = Ash.Resource.Info.relationship(resource, name)

    {relationship, relationship.context,
     relationship.read_action ||
       Ash.Resource.Info.primary_action!(relationship.destination, :read)}
  end

  defp last_relationship_context_and_action(resource, path) do
    second_to_last = Ash.Resource.Info.related(resource, :lists.droplast(path))

    relationship = Ash.Resource.Info.relationship(second_to_last, List.last(path))

    {relationship, relationship.context, relationship.read_action}
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
         domain,
         %{type: :many_to_many, join_relationship: join_relationship, through: through} =
           relationship,
         tenant
       ) do
    if Ash.DataLayer.data_layer(through) == Ash.DataLayer.data_layer(resource) &&
         Ash.DataLayer.data_layer_can?(resource, {:join, through}) do
      filter = %__MODULE__{
        resource: relationship.destination,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(domain: domain)
      |> Ash.Query.do_filter(filter)
      |> filter_related_in(
        relationship,
        :lists.droplast(path) ++ [join_relationship],
        domain,
        tenant
      )
    else
      filter = %__MODULE__{
        resource: through,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(domain: ShadowDomain)
      |> Ash.Query.do_filter(filter)
      |> Ash.Query.do_filter(relationship.filter, parent_stack: [relationship.source])
      |> Ash.Query.sort(relationship.sort, prepend?: true)
      |> Ash.Query.set_context(relationship.context)
      |> Ash.Actions.Read.unpaginated_read()
      |> case do
        {:ok, results} ->
          relationship.through
          |> Ash.Query.new(domain: domain)
          |> Ash.Query.do_filter([
            {relationship.destination_attribute_on_join_resource,
             in: Enum.map(results, &Map.get(&1, relationship.destination_attribute))}
          ])
          |> filter_related_in(
            Ash.Resource.Info.relationship(resource, join_relationship),
            :lists.droplast(path),
            domain,
            tenant
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
         domain,
         relationship,
         tenant
       ) do
    filter = %__MODULE__{
      resource: relationship.destination,
      expression: new_predicate
    }

    relationship.destination
    |> Ash.Query.new(domain: domain)
    |> Ash.Query.do_filter(filter)
    |> Ash.Query.do_filter(relationship.filter, parent_stack: [relationship.source])
    |> Ash.Query.sort(relationship.sort, prepend?: true)
    |> Ash.Query.set_context(relationship.context)
    |> Ash.Query.set_context(%{private: %{internal?: true}})
    |> filter_related_in(relationship, :lists.droplast(path), domain, tenant)
  end

  defp filter_related_in(
         query,
         relationship,
         path,
         _domain,
         tenant
       ) do
    query = Ash.Query.set_tenant(query, tenant)

    case Ash.Actions.Read.unpaginated_read(query) do
      {:ok, data} ->
        {:ok,
         records_to_expression(
           data,
           relationship,
           path
         )}

      {:error, error} ->
        {:error, error}
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
    Enum.reduce_while(records, {:ok, true}, fn record, {:ok, expression} ->
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

  def relationship_paths(
        filter_or_expression,
        include_exists? \\ false,
        with_refs? \\ false,
        expand_aggregates? \\ false
      )

  def relationship_paths(nil, _, _, _), do: []
  def relationship_paths(%__MODULE__{expression: nil}, _, _, _), do: []

  def relationship_paths(
        %__MODULE__{expression: expression},
        include_exists?,
        with_refs?,
        expand_aggregates?
      ),
      do: relationship_paths(expression, include_exists?, with_refs?, expand_aggregates?)

  def relationship_paths(expression, include_exists?, with_refs?, expand_aggregates?) do
    paths =
      expression
      |> do_relationship_paths(include_exists?, with_refs?, expand_aggregates?)
      |> List.wrap()
      |> List.flatten()

    if with_refs? do
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

  defp do_relationship_paths(
         %Ref{
           relationship_path: path,
           resource: resource,
           attribute: %Ash.Query.Calculation{module: module, opts: opts, context: context}
         } = ref,
         include_exists?,
         with_references?,
         expand_aggregates?
       ) do
    if module.has_expression?() do
      expression = module.expression(opts, context)

      case hydrate_refs(expression, %{
             resource: resource,
             relationship_path: [],
             public?: false
           }) do
        {:ok, expression} ->
          path_and_ref =
            if with_references? do
              {path, ref}
            else
              {path}
            end

          nested =
            expression
            |> do_relationship_paths(include_exists?, with_references?, expand_aggregates?)
            |> List.wrap()
            |> List.flatten()

          # This validation needs to be added back in at some point
          # it was removed because we currently can't tell the difference between relationship paths
          # that appear inside of the path to `exists`, i.e `exists(to_many, ....)`.
          # nested
          # |> Enum.map(fn
          #   {nested_path, _ref} ->
          #     nested_path

          #   {nested_path} ->
          #     nested_path
          # end)
          # |> Enum.find(fn path ->
          #   not to_one_path?(path, resource)
          # end)
          # |> case do
          #   nil ->
          #     :ok

          #   path ->
          # raise """
          # Only to-one relationship references are allowed in a calculation reference paths.
          # Got: #{inspect(Enum.join(path, "."))} in #{inspect(ref)}

          # To extract a single value from a to_many relationship or path that includes a
          # to_many relationship, use a `first` aggregate.
          # """
          # end

          nested =
            Enum.map(nested, fn
              {nested_path, ref} ->
                {path ++ nested_path, ref}

              {nested_path} ->
                {path ++ nested_path}
            end)

          [path_and_ref | nested]

        _ ->
          if with_references? do
            [{path, ref}]
          else
            [{path}]
          end
      end
    else
      if with_references? do
        [{path, ref}]
      else
        [{path}]
      end
    end
  end

  defp do_relationship_paths(
         %Ref{
           relationship_path: path,
           attribute: %Ash.Query.Aggregate{} = aggregate
         } = ref,
         include_exists?,
         with_refs?,
         true
       ) do
    this_agg_ref =
      if with_refs? do
        {path, ref}
      else
        {path}
      end

    [this_agg_ref | aggregate_refs(path, aggregate, include_exists?, with_refs?)]
  end

  defp do_relationship_paths(%Ref{relationship_path: path} = ref, _, true, _) do
    [{path, ref}]
  end

  defp do_relationship_paths(%Ref{relationship_path: path}, _, false, _) do
    [{path}]
  end

  defp do_relationship_paths(
         %BooleanExpression{left: left, right: right},
         include_exists?,
         with_refs?,
         expand_aggregates?
       ) do
    do_relationship_paths(left, include_exists?, with_refs?, expand_aggregates?) ++
      do_relationship_paths(right, include_exists?, with_refs?, expand_aggregates?)
  end

  defp do_relationship_paths(
         %Not{expression: expression},
         include_exists?,
         with_refs?,
         expand_aggregates?
       ) do
    do_relationship_paths(expression, include_exists?, with_refs?, expand_aggregates?)
  end

  defp do_relationship_paths(
         %Ash.Query.Exists{at_path: at_path},
         false,
         with_refs?,
         _expand_aggregates?
       ) do
    if with_refs? do
      []
    else
      [{at_path}]
    end
  end

  defp do_relationship_paths(
         %Ash.Query.Exists{path: path, expr: expression, at_path: at_path},
         include_exists?,
         false,
         expand_aggregates?
       ) do
    expression
    |> do_relationship_paths(include_exists?, false, expand_aggregates?)
    |> List.flatten()
    |> Enum.flat_map(fn {rel_path} ->
      [{at_path}, {at_path ++ path ++ rel_path}]
    end)
    |> Kernel.++(
      parent_relationship_paths(expression, at_path, include_exists?, false, expand_aggregates?)
    )
  end

  defp do_relationship_paths(
         %Ash.Query.Exists{path: path, expr: expression, at_path: at_path},
         include_exists?,
         true,
         expand_aggregates?
       ) do
    expression
    |> do_relationship_paths(include_exists?, true, expand_aggregates?)
    |> List.flatten()
    |> Enum.flat_map(fn {rel_path, ref} ->
      [{at_path ++ path ++ rel_path, ref}]
    end)
    |> Kernel.++(
      parent_relationship_paths(expression, at_path, include_exists?, true, expand_aggregates?)
    )
  end

  defp do_relationship_paths(
         %{__operator__?: true, left: left, right: right},
         include_exists?,
         with_refs?,
         expand_aggregates?
       ) do
    Enum.flat_map(
      [left, right],
      &do_relationship_paths(&1, include_exists?, with_refs?, expand_aggregates?)
    )
  end

  defp do_relationship_paths({key, value}, include_exists?, with_refs?, expand_aggregates?)
       when is_atom(key) do
    do_relationship_paths(value, include_exists?, with_refs?, expand_aggregates?)
  end

  defp do_relationship_paths(
         %{__function__?: true, arguments: arguments},
         include_exists?,
         with_refs?,
         expand_aggregates?
       ) do
    Enum.flat_map(
      arguments,
      &do_relationship_paths(&1, include_exists?, with_refs?, expand_aggregates?)
    )
  end

  defp do_relationship_paths(value, include_exists?, with_refs?, expand_aggregates?)
       when is_list(value) do
    Enum.flat_map(
      value,
      &do_relationship_paths(&1, include_exists?, with_refs?, expand_aggregates?)
    )
  end

  defp do_relationship_paths(value, include_exists?, with_references?, expand_aggregates?)
       when is_map(value) and not is_struct(value) do
    Enum.flat_map(value, fn {key, value} ->
      do_relationship_paths(key, include_exists?, with_references?, expand_aggregates?) ++
        do_relationship_paths(value, include_exists?, with_references?, expand_aggregates?)
    end)
  end

  defp do_relationship_paths(_, _, _, _), do: []

  defp aggregate_refs(path, aggregate, include_exists?, with_refs?) do
    query_rel_paths =
      if aggregate.query && aggregate.query.filter do
        aggregate.query.filter
        |> do_relationship_paths(include_exists?, with_refs?, true)
      else
        []
      end

    if aggregate.field do
      related = Ash.Resource.Info.related(aggregate.resource, aggregate.relationship_path)

      field_ref =
        case aggregate.field do
          field when is_atom(field) ->
            Ash.Resource.Info.field(related, aggregate.field)

          field ->
            field
        end

      field_ref = field_to_ref(aggregate.resource, field_ref)

      query_rel_paths ++ do_relationship_paths(field_ref, include_exists?, with_refs?, true)
    else
      query_rel_paths
    end
    |> Enum.map(fn
      {agg_path} ->
        {path ++ aggregate.relationship_path ++ agg_path}

      {agg_path, ref} ->
        {path ++ aggregate.relationship_path ++ agg_path,
         %{
           ref
           | relationship_path: path ++ aggregate.relationship_path ++ ref.relationship_path,
             input?: true
         }}
    end)
  end

  defp field_to_ref(resource, %Ash.Resource.Attribute{} = attr) do
    %Ref{
      resource: resource,
      attribute: attr,
      relationship_path: []
    }
  end

  defp field_to_ref(resource, %Ash.Resource.Aggregate{} = aggregate) do
    related = Ash.Resource.Info.related(resource, aggregate.relationship_path)

    read_action =
      aggregate.read_action || Ash.Resource.Info.primary_action!(related, :read).name

    with %{valid?: true} = aggregate_query <- Ash.Query.for_read(related, read_action),
         %{valid?: true} = aggregate_query <-
           Ash.Query.Aggregate.build_query(aggregate_query,
             filter: aggregate.filter,
             sort: aggregate.sort
           ) do
      case Aggregate.new(
             resource,
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
             read_action: read_action,
             authorize?: aggregate.authorize?,
             join_filters: Map.new(aggregate.join_filters, &{&1.relationship_path, &1.filter})
           ) do
        {:ok, query_aggregate} ->
          query_aggregate = %{query_aggregate | load: aggregate.name}
          field_to_ref(resource, query_aggregate)

        {:error, error} ->
          raise "Could not construct aggregate #{inspect(aggregate)}: #{inspect(error)}"
      end
    else
      %{errors: errors} ->
        raise "Could not construct aggregate #{inspect(aggregate)}: #{inspect(errors)}"
    end
  end

  defp field_to_ref(resource, %Ash.Resource.Calculation{} = calc) do
    {module, opts} = calc.calculation

    case Calculation.new(
           calc.name,
           module,
           opts,
           calc.type,
           calc.constraints,
           filterable?: calc.filterable?,
           sortable?: calc.sortable?,
           sensitive?: calc.sensitive?
         ) do
      {:ok, calc} ->
        field_to_ref(resource, calc)

      {:error, error} ->
        raise "Could not construct calculation #{inspect(calc)}: #{inspect(error)}"
    end
  end

  defp field_to_ref(resource, field) do
    %Ref{
      resource: resource,
      attribute: field,
      relationship_path: []
    }
  end

  defp parent_relationship_paths(
         expression,
         at_path,
         include_exists?,
         with_refs?,
         expand_aggregates?
       ) do
    expression
    |> flat_map(fn
      %Ash.Query.Parent{expr: expr} ->
        expr
        |> do_relationship_paths(include_exists?, with_refs?, expand_aggregates?)
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

  def list_refs(
        expression,
        no_longer_simple? \\ false,
        in_an_eq? \\ false,
        expand_calculations? \\ false,
        expand_get_path? \\ false
      ) do
    expression
    |> do_list_refs(no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?)
    |> Enum.uniq()
  end

  defp do_list_refs(list, no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?)
       when is_list(list) do
    Enum.flat_map(
      list,
      &do_list_refs(&1, no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?)
    )
  end

  defp do_list_refs(
         {key, value},
         no_longer_simple?,
         in_an_eq?,
         expand_calculations?,
         expand_get_path?
       )
       when is_atom(key),
       do:
         do_list_refs(value, no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?)

  defp do_list_refs(
         %__MODULE__{expression: expression},
         no_longer_simple?,
         in_an_eq?,
         expand_calculations?,
         expand_get_path?
       ) do
    do_list_refs(expression, no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?)
  end

  defp do_list_refs(
         expression,
         no_longer_simple?,
         in_an_eq?,
         expand_calculations?,
         expand_get_path?
       ) do
    case expression do
      %Call{name: :get_path, args: [%Ref{} = ref, path]} when expand_get_path? == true ->
        expand_get_path_refs(ref, path, expand_calculations?)

      %Ash.Query.Function.GetPath{arguments: [%Ref{} = ref, path]}
      when expand_get_path? == true ->
        expand_get_path_refs(ref, path, expand_calculations?)

      %BooleanExpression{left: left, right: right, op: op} ->
        no_longer_simple? = no_longer_simple? || op == :or

        do_list_refs(left, no_longer_simple?, false, expand_calculations?, expand_get_path?) ++
          do_list_refs(right, no_longer_simple?, false, expand_calculations?, expand_get_path?)

      %Not{expression: not_expr} ->
        do_list_refs(not_expr, true, false, expand_calculations?, expand_get_path?)

      %struct{__predicate__?: _, left: left, right: right} ->
        in_an_eq? = struct == Ash.Query.Operator.Eq

        do_list_refs(left, no_longer_simple?, in_an_eq?, expand_calculations?, expand_get_path?) ++
          do_list_refs(
            right,
            no_longer_simple?,
            in_an_eq?,
            expand_calculations?,
            expand_get_path?
          )

      %{__predicate__?: _, arguments: args} ->
        Enum.flat_map(
          args,
          &do_list_refs(&1, true, false, expand_calculations?, expand_get_path?)
        )

      value when is_list(value) ->
        Enum.flat_map(
          value,
          &do_list_refs(&1, true, false, expand_calculations?, expand_get_path?)
        )

      value when is_map(value) and not is_struct(value) ->
        Enum.flat_map(value, fn {key, value} ->
          do_list_refs(key, true, false, expand_calculations?, expand_get_path?) ++
            do_list_refs(value, true, false, expand_calculations?, expand_get_path?)
        end)

      %Ash.Query.Exists{at_path: at_path, path: path, expr: expr} ->
        parent_refs_inside_of_exists =
          flat_map(expr, fn
            %Ash.Query.Parent{expr: expr} ->
              expr
              |> do_list_refs(true, false, expand_calculations?, expand_get_path?)
              |> Enum.map(&%{&1 | relationship_path: at_path ++ &1.relationship_path})

            _ ->
              []
          end)

        expr
        |> do_list_refs(true, false, expand_calculations?, expand_get_path?)
        |> Enum.map(&%{&1 | relationship_path: at_path ++ path ++ &1.relationship_path})
        |> Enum.concat(parent_refs_inside_of_exists)

      %Call{args: args, relationship_path: relationship_path} ->
        args
        |> Enum.flat_map(&do_list_refs(&1, true, false, expand_calculations?, expand_get_path?))
        |> Enum.map(&%{&1 | relationship_path: relationship_path ++ &1.relationship_path})

      %Ref{
        attribute: %Calculation{module: module, opts: opts, context: context},
        relationship_path: calc_relationship_path
      } = ref ->
        if expand_calculations? && module.has_expression?() do
          expression = module.expression(opts, context)

          case hydrate_refs(expression, %{
                 resource: ref.resource,
                 relationship_path: [],
                 public?: false
               }) do
            {:ok, expression} ->
              nested_refs =
                expression
                |> do_list_refs(true, false, expand_calculations?, expand_get_path?)
                |> Enum.map(fn ref ->
                  %{ref | relationship_path: calc_relationship_path ++ ref.relationship_path}
                end)

              [%{ref | simple_equality?: !no_longer_simple? && in_an_eq?} | nested_refs]

            _ ->
              [%{ref | simple_equality?: !no_longer_simple? && in_an_eq?}]
          end
        else
          [%{ref | simple_equality?: !no_longer_simple? && in_an_eq?}]
        end

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

  defp expand_get_path_refs(ref, path, expand_calculations?) do
    if is_list(path) && Enum.all?(path, &(is_atom(&1) || is_binary(&1))) do
      path =
        Enum.map(path, fn item ->
          if is_binary(item) do
            String.to_existing_atom(item)
          else
            item
          end
        end)

      attribute = List.last(path)
      path = :lists.droplast(path)

      [
        %{
          ref
          | relationship_path: ref.relationship_path ++ [ref.attribute] ++ path,
            attribute: attribute
        }
      ]
    else
      do_list_refs(ref, true, false, expand_calculations?, false)
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
    Enum.reduce_while(statement, {:ok, true}, fn expression_part, {:ok, expression} ->
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

  defp add_expression_part(boolean, context, nil) do
    add_expression_part(boolean, context, true)
  end

  defp add_expression_part(boolean, _context, expression) when is_boolean(boolean) do
    {:ok, BooleanExpression.optimized_new(:and, expression, boolean)}
  end

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

  defp add_expression_part(%Ash.CustomExpression{} = custom, _context, expression) do
    {:ok, BooleanExpression.optimized_new(:and, expression, custom)}
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
         NoSuchField.exception(
           field: List.first(ref.relationship_path),
           resource: context.resource
         )}

      related ->
        new_context = %{
          relationship_path: ref.relationship_path,
          resource: related,
          root_resource: context.root_resource,
          public?: context.public?
        }

        case ref.attribute do
          %Ash.Query.Calculation{} = calc ->
            add_expression_part({calc, nested_statement}, new_context, expression)

          %Ash.Query.Aggregate{} = agg ->
            add_expression_part({agg, nested_statement}, new_context, expression)

          %{name: name} ->
            add_expression_part({name, nested_statement}, new_context, expression)
        end
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

  defp add_expression_part(%_{__predicate__?: _} = pred, context, expression) do
    {:ok,
     BooleanExpression.optimized_new(
       :and,
       expression,
       move_to_relationship_path(pred, context[:relationship_path] || [])
     )}
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
       when is_tuple(args) and (is_atom(function) or is_binary(function)) do
    case get_function(function, context.resource, context.public?) do
      nil ->
        case calculation(context, function) do
          nil ->
            add_expression_part({function, [args]}, context, expression)

          resource_calculation when tuple_size(args) == 2 ->
            {module, opts} = resource_calculation.calculation
            {args, nested_statement} = args

            with {:ok, args} <-
                   Ash.Query.validate_calculation_arguments(resource_calculation, args || %{}),
                 {:ok, calculation} <-
                   Calculation.new(
                     resource_calculation.name,
                     module,
                     opts,
                     resource_calculation.type,
                     resource_calculation.constraints,
                     arguments: args,
                     filterable?: resource_calculation.filterable?,
                     sortable?: resource_calculation.sortable?,
                     sensitive?: resource_calculation.sensitive?,
                     load: resource_calculation.load
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

        with {:ok, args} <- hydrate_refs(List.wrap(nested_statement), context),
             refs <- list_refs(args),
             :ok <- validate_refs(refs, context.root_resource, {function, nested_statement}),
             {:ok, function} <- Function.new(function_module, args) do
          if is_nil(context.resource) ||
               Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
            {:ok, BooleanExpression.optimized_new(:and, expression, function)}
          else
            {:error, "data layer does not support the function #{inspect(function)}"}
          end
        end
    end
  end

  defp add_expression_part({field, nested_statement}, context, expression)
       when is_atom(field) or is_binary(field) do
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
                   "a single value must be castable to the primary key of the resource: #{inspect(context.resource)}"
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
               Ash.Query.Aggregate.build_query(aggregate_query,
                 filter: aggregate.filter,
                 sort: aggregate.sort
               ),
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
                 sortable?: aggregate.sortable?,
                 sensitive?: aggregate.sensitive?,
                 type: aggregate.type,
                 constraints: aggregate.constraints,
                 implementation: aggregate.implementation,
                 uniq?: aggregate.uniq?,
                 read_action: read_action,
                 authorize?: aggregate.authorize?,
                 join_filters: Map.new(aggregate.join_filters, &{&1.relationship_path, &1.filter})
               ) do
          query_aggregate = %{query_aggregate | load: aggregate.name}

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

      resource_calculation = calculation(context, field) ->
        {module, opts} = resource_calculation.calculation

        {input, nested_statement} =
          case nested_statement do
            %{"input" => input} ->
              {input, Map.delete(nested_statement, "input")}

            %{input: input} ->
              {input, Map.delete(nested_statement, :input)}

            _ ->
              {%{}, nested_statement}
          end

        with {:ok, args} <-
               Ash.Query.validate_calculation_arguments(
                 resource_calculation,
                 input,
                 !context[:input?]
               ),
             {:ok, calculation} <-
               Calculation.new(
                 resource_calculation.name,
                 module,
                 opts,
                 resource_calculation.type,
                 resource_calculation.constraints,
                 arguments: args,
                 filterable?: resource_calculation.filterable?,
                 sortable?: resource_calculation.sortable?,
                 sensitive?: resource_calculation.sensitive?,
                 load: resource_calculation.load
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
        with {:ok, [left, right]} <- hydrate_refs(nested_statement, context),
             refs <- list_refs([left, right]),
             :ok <- validate_refs(refs, context.root_resource, {field, nested_statement}),
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
         NoSuchField.exception(
           field: field,
           resource: context.resource
         )}
    end
  end

  defp add_expression_part({%{__struct__: field_struct} = calc, rest}, context, expression)
       when field_struct in [Ash.Query.Calculation, Ash.Query.Aggregate] do
    case parse_predicates(rest, calc, context) do
      {:ok, nested_statement} ->
        {:ok, BooleanExpression.optimized_new(:and, expression, nested_statement)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(%{__struct__: field_struct} = calc, _context, expression)
       when field_struct in [Ash.Query.Calculation, Ash.Query.Aggregate] do
    {:ok, BooleanExpression.optimized_new(:and, calc, expression)}
  end

  defp add_expression_part(value, context, expression) when is_map(value) do
    # Can't call `parse_expression/2` here because it will loop

    value
    |> Map.to_list()
    |> Enum.reduce_while({:ok, true}, fn {key, value}, {:ok, expression} ->
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

      Logger.warning(
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
               "cannot access multiple resources for a data layer that can't be joined from within a single expression"
           )}
        end

      [_ | _] ->
        {:error,
         Ash.Error.Query.InvalidExpression.exception(
           expression: expr,
           message: "cannot access multiple data layers within a single expression"
         )}
    end
  end

  defp resolve_call(
         %Call{name: name, args: args, operator?: true, relationship_path: relationship_path} =
           call,
         context
       ) do
    with :ok <- validate_datalayer_supports_nested_expressions(args, context.resource),
         {:op, op_module} when not is_nil(op_module) <- {:op, get_operator(name)},
         context <-
           Map.merge(context, %{
             resource: Ash.Resource.Info.related(context.resource, relationship_path),
             relationship_path: []
           }),
         {:ok, [left, right]} <- hydrate_refs(args, context),
         refs <- list_refs([left, right]),
         :ok <- validate_refs(refs, context.root_resource, call),
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
        {:error, NoSuchOperator.exception(operator: name)}

      other ->
        other
    end
  end

  defp resolve_call(
         %Call{name: :parent, args: [arg], relationship_path: []},
         context
       ) do
    do_hydrate_refs(%Ash.Query.Parent{expr: arg}, context)
  end

  defp resolve_call(%Call{name: name, args: args} = call, context)
       when name in @inline_aggregates do
    resource = Ash.Resource.Info.related(context.resource, call.relationship_path)
    path = refs_to_path(Enum.at(args, 0))
    related = Ash.Resource.Info.related(resource, path)

    if !related do
      raise "Expression `#{inspect(call)}` is invalid. `#{inspect(Enum.at(args, 0))}` is not a valid relationship path from #{inspect(resource)}."
    end

    opts = Enum.at(args, 1) || []

    if Keyword.keyword?(opts) do
      kind =
        if name == :custom_aggregate do
          :custom
        else
          name
        end

      field =
        if opts[:field] do
          opts[:field]
        else
          unless kind == :custom do
            List.first(Ash.Resource.Info.primary_key(related))
          end
        end

      opts =
        if field && kind != :custom do
          attribute = Ash.Resource.Info.attribute(related, field)

          if attribute do
            {:ok, type, constraints} =
              Ash.Query.Aggregate.kind_to_type(kind, attribute.type, attribute.constraints)

            opts
            |> Keyword.put(:type, type)
            |> Keyword.put(:constraints, constraints)
          else
            opts
          end
        else
          opts
        end

      opts = Keyword.put(opts, :path, path)

      with {:ok, agg} <-
             Ash.Query.Aggregate.new(
               resource,
               agg_name(kind, opts),
               kind,
               opts
             ) do
        {:ok,
         %Ref{
           relationship_path: call.relationship_path,
           attribute: agg
         }}
      end
    else
      {:error, "Aggregate options must be keyword list. In: #{inspect(call)}"}
    end
  end

  defp resolve_call(%Call{name: name, args: args} = call, context) do
    could_be_calculation? = Enum.count_until(args, 2) == 1 && Keyword.keyword?(Enum.at(args, 0))

    resource =
      if context.resource do
        Ash.Resource.Info.related(context.resource, call.relationship_path)
      end

    context =
      Map.merge(context, %{
        resource: resource,
        relationship_path: []
      })

    resource_calculation =
      if could_be_calculation? do
        calculation(%{context | resource: resource}, name)
      end

    cond do
      resource_calculation ->
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
                 resource_calculation.type,
                 resource_calculation.constraints,
                 arguments: args,
                 filterable?: resource_calculation.filterable?,
                 sortable?: resource_calculation.sortable?,
                 sensitive?: resource_calculation.sensitive?,
                 load: resource_calculation.load
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

      custom_expression =
          custom_expression(name, args) ->
        {module, arguments} = custom_expression

        data_layer =
          if resource do
            Ash.Resource.Info.data_layer(resource)
          else
            Ash.DataLayer.Simple
          end

        with {:ok, expr} <- module.expression(data_layer, arguments),
             {:ok, expr} <- hydrate_refs(expr, context) do
          if data_layer == Ash.DataLayer.Simple do
            {:ok,
             %Ash.CustomExpression{
               arguments: arguments,
               expression: expr,
               simple_expression: {:ok, expr}
             }}
          else
            with {:ok, simple_expr} <- module.expression(Ash.DataLayer.Simple, arguments),
                 {:ok, simple_expr} <- hydrate_refs(simple_expr, context) do
              {:ok,
               %Ash.CustomExpression{
                 arguments: arguments,
                 expression: expr,
                 simple_expression: {:ok, simple_expr}
               }}
            else
              {:error, error} ->
                {:error, error}

              :unknown ->
                {:ok,
                 %Ash.CustomExpression{
                   arguments: arguments,
                   expression: expr,
                   simple_expression: :unknown
                 }}
            end
          end
        else
          {:error, error} ->
            {:error, error}

          :unknown ->
            {:error,
             "Custom expression: `#{inspect(module)}` returned `:unknown` for data layer `#{inspect(data_layer)}` for arguments `#{inspect(arguments)}`"}
        end

      true ->
        with :ok <- validate_datalayer_supports_nested_expressions(args, context.resource),
             {:ok, args} <- hydrate_refs(args, context),
             refs <- list_refs(args),
             :ok <- validate_refs(refs, context.root_resource, call),
             {:func, function_module} when not is_nil(function_module) <-
               {:func, get_function(name, context.resource, context.public?)},
             {:ok, function} <- Function.new(function_module, args) do
          if Ash.Expr.expr?(function) && !match?(%{__predicate__?: _}, function) do
            hydrate_refs(function, context)
          else
            if is_nil(context.resource) ||
                 Ash.DataLayer.data_layer_can?(context.resource, {:filter_expr, function}) do
              {:ok, function}
            else
              function.arguments
              |> List.wrap()
              |> Enum.reduce_while({:ok, []}, fn arg, {:ok, acc} ->
                case Ash.Expr.eval(arg) do
                  {:ok, value} ->
                    {:cont, {:ok, [value | acc]}}

                  _ ->
                    {:halt,
                     {:error, "data layer does not support the function #{inspect(function)}"}}
                end
              end)
              |> case do
                {:ok, arguments} ->
                  function = %{function | arguments: Enum.reverse(arguments)}

                  case function_module.evaluate(function) do
                    {:known, result} ->
                      {:ok, result}

                    _ ->
                      {:error, "data layer does not support the function #{inspect(function)}"}
                  end

                {:error, error} ->
                  {:error, error}
              end
            end
          end
        else
          {:func, nil} ->
            {:error, NoSuchFunction.exception(function: name, resource: context.resource)}

          other ->
            other
        end
    end
  end

  # This kind of sucks, but anonymous aggregates need consistent names currently.
  # We may want to move this into the data layer to be responsible for setting these
  # names
  defp agg_name(kind, opts) do
    opts_string =
      opts
      |> Keyword.take([
        :query,
        :field,
        :default,
        :filterable?,
        :type,
        :constraints,
        :implementation,
        :read_action,
        :uniq?,
        :authorize?
      ])
      |> Keyword.reject(fn {_key, value} -> is_nil(value) end)
      |> case do
        [] ->
          ""

        opts ->
          inspect(opts)
      end

    "#{kind}(#{Enum.join(opts[:path] || [], ".")}#{opts_string})"
  end

  defp refs_to_path(%Ref{relationship_path: relationship_path, attribute: attribute}) do
    attribute =
      case attribute do
        %{name: name} ->
          name

        name ->
          name
      end

    relationship_path ++ [attribute]
  end

  defp refs_to_path(list) when is_list(list) do
    Enum.flat_map(list, fn item -> refs_to_path(item) end)
  end

  defp refs_to_path(item), do: [item]

  defp validate_datalayer_supports_nested_expressions(args, resource) do
    if resource && Enum.any?(args, &Ash.Expr.expr?/1) &&
         !Ash.DataLayer.data_layer_can?(resource, :nested_expressions) do
      {:error, "Datalayer does not support nested expressions"}
    else
      :ok
    end
  end

  def custom_expression(name, args) do
    with module when not is_nil(module) <- Enum.find(@custom_expressions, &(&1.name() == name)),
         args when not is_nil(args) <-
           Enum.find_value(
             module.arguments(),
             &Ash.Query.Function.try_cast_arguments(&1, args)
           ) do
      {module, args}
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

  def do_hydrate_refs({:_ref, value}, context) do
    do_hydrate_refs(
      %Ash.Query.Ref{
        attribute: value,
        relationship_path: [],
        input?: Map.get(context, :input?, false),
        resource: context.root_resource
      },
      context
    )
  end

  def do_hydrate_refs({:_ref, path, value}, context) do
    do_hydrate_refs(
      %Ash.Query.Ref{
        attribute: value,
        relationship_path: path,
        input?: Map.get(context, :input?, false),
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
        %Ref{relationship_path: relationship_path, resource: nil} = ref,
        %{resource: resource} = context
      )
      when not is_nil(resource) do
    case Ash.Resource.Info.related(resource, relationship_path || []) do
      nil ->
        {:error, "Invalid reference #{inspect(ref)}"}

      related ->
        do_hydrate_refs(
          %{ref | resource: related},
          context
        )
    end
  end

  def do_hydrate_refs(
        %Ref{attribute: attribute} = ref,
        context
      )
      when is_atom(attribute) or is_binary(attribute) do
    ref = %{ref | input?: ref.input? || context[:input?] || false}

    case related(context, ref.relationship_path) do
      nil ->
        {:error,
         "Invalid reference #{inspect(ref)} at relationship_path #{inspect(ref.relationship_path)}"}

      related ->
        context = %{context | resource: related}

        cond do
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
                     resource_calculation.type,
                     resource_calculation.constraints,
                     arguments: args,
                     filterable?: resource_calculation.filterable?,
                     sortable?: resource_calculation.sortable?,
                     sensitive?: resource_calculation.sensitive?,
                     load: resource_calculation.load
                   ) do
              {:ok, %{ref | attribute: calculation, resource: related}}
            else
              {:error, error} ->
                {:error, error}
            end

          aggregate = aggregate(context, attribute) ->
            agg_related = Ash.Resource.Info.related(related, aggregate.relationship_path)

            with %{valid?: true} = aggregate_query <-
                   Ash.Query.new(agg_related),
                 %{valid?: true} = aggregate_query <-
                   Ash.Query.Aggregate.build_query(aggregate_query,
                     filter: aggregate.filter,
                     sort: aggregate.sort
                   ),
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
                     sortable?: aggregate.sortable?,
                     sensitive?: aggregate.sensitive?,
                     constraints: aggregate.constraints,
                     implementation: aggregate.implementation,
                     uniq?: aggregate.uniq?,
                     read_action: aggregate.read_action,
                     authorize?: aggregate.authorize?,
                     join_filters:
                       Map.new(aggregate.join_filters, &{&1.relationship_path, &1.filter})
                   ) do
              query_aggregate = %{query_aggregate | load: aggregate.name}

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
    ref = %{ref | input?: ref.input? || context[:input?] || false}
    {:ok, %{ref | resource: Ash.Resource.Info.related(context.resource, relationship_path)}}
  end

  def do_hydrate_refs(%BooleanExpression{left: left, right: right} = expr, context) do
    case do_hydrate_refs(left, context) do
      {:ok, true} ->
        if expr.op == :or do
          true
        else
          do_hydrate_refs(right, context)
        end

      {:ok, false} ->
        if expr.op == :or do
          do_hydrate_refs(right, context)
        else
          {:ok, false}
        end

      {:ok, left} ->
        case do_hydrate_refs(right, context) do
          {:ok, true} ->
            if expr.op == :or do
              true
            else
              {:ok, left}
            end

          {:ok, false} ->
            if expr.op == :or do
              {:ok, left}
            else
              {:ok, false}
            end

          {:ok, right} ->
            {:ok, %{expr | left: left, right: right}}
        end
    end
  end

  def do_hydrate_refs(%Not{expression: expression} = expr, context) do
    with {:ok, expression} <- do_hydrate_refs(expression, context) do
      case expression do
        true ->
          {:ok, false}

        false ->
          {:ok, true}

        _ ->
          {:ok, %{expr | expression: expression}}
      end
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
    if !Map.has_key?(context, :parent_stack) || context.parent_stack in [[], nil] do
      {:ok, this}
    else
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
      public?: context[:public?],
      input?: context[:input?],
      data_layer: Ash.DataLayer.data_layer(new_resource)
    }

    case do_hydrate_refs(expr, context) do
      {:ok, expr} ->
        {:ok, %{exists | expr: expr}}

      other ->
        other
    end
  end

  def do_hydrate_refs(%__MODULE__{expression: expression} = filter, context) do
    case do_hydrate_refs(expression, context) do
      {:ok, expression} ->
        {:ok, %{filter | expression: expression}}

      {:error, error} ->
        {:error, error}
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

  def do_hydrate_refs(map, context) when is_map(map) and not is_struct(map) do
    map
    |> Enum.reduce_while({:ok, %{}}, fn {key, value}, {:ok, acc} ->
      with {:ok, key} <- do_hydrate_refs(key, context),
           {:ok, value} <- do_hydrate_refs(value, context) do
        {:cont, {:ok, Map.put(acc, key, value)}}
      else
        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  def do_hydrate_refs(%Ref{} = ref, context) do
    {:ok, %{ref | input?: ref.input? || context[:input?] || false}}
  end

  def do_hydrate_refs(val, _context) do
    {:ok, val}
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

      %Ash.Query.Exists{} = exists ->
        %{exists | at_path: relationship_path ++ exists.at_path}

      %Call{relationship_path: call_path} = call ->
        %{call | relationship_path: relationship_path ++ call_path}

      %__MODULE__{expression: expression} = filter ->
        %{filter | expression: move_to_relationship_path(expression, relationship_path)}

      list when is_list(list) ->
        Enum.map(list, &move_to_relationship_path(&1, relationship_path))

      value when is_map(value) and not is_struct(value) ->
        Map.new(value, fn {key, value} ->
          {move_to_relationship_path(key, relationship_path),
           move_to_relationship_path(value, relationship_path)}
        end)

      other ->
        other
    end
  end

  defp add_to_ref_path(%Ref{relationship_path: relationship_path} = ref, to_add) do
    %{ref | relationship_path: to_add ++ relationship_path}
  end

  defp add_to_ref_path(other, _), do: other

  defp parse_and_join([statement | statements], op, context) do
    case parse_expression(statement, context) do
      {:ok, nested_expression} ->
        Enum.reduce_while(statements, {:ok, nested_expression}, fn statement, {:ok, expression} ->
          case parse_expression(statement, context) do
            {:ok, nested_expression} ->
              {:cont, {:ok, BooleanExpression.optimized_new(op, expression, nested_expression)}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        end)

      {:error, error} ->
        {:error, error}
    end
  end

  defp parse_predicates(value, field, context)
       when not is_list(value) and not is_map(value) do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(value, field, context) when value == %{} do
    parse_predicates([eq: true], field, context)
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
        at_path =
          if is_map(values) do
            Map.get(values, :at_path) || Map.get(values, "at_path")
          else
            Keyword.get(values, :at_path)
          end

        {values, at_path} =
          if is_list(at_path) do
            if is_map(values) do
              {Map.drop(values, [:at_path, "at_path"]), at_path}
            else
              {Keyword.delete(values, :at_path), at_path}
            end
          else
            {values, nil}
          end

        Enum.reduce_while(values, {:ok, true}, fn
          {key, value}, {:ok, expression} when key in [:not, "not"] ->
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
                case get_predicate_function(key, context.resource, context.public?) do
                  nil ->
                    error = NoSuchFilterPredicate.exception(key: key, resource: context.resource)
                    {:halt, {:error, error}}

                  function_module ->
                    left =
                      if is_list(at_path) do
                        %Call{
                          name: :get_path,
                          args: [
                            %Ref{
                              attribute: attr,
                              relationship_path: context[:relationship_path] || [],
                              resource: context.resource,
                              input?: true
                            },
                            at_path
                          ]
                        }
                      else
                        %Ref{
                          attribute: attr,
                          relationship_path: context[:relationship_path] || [],
                          resource: context.resource,
                          input?: true
                        }
                      end

                    with {:ok, args} <- hydrate_refs([left, value], context),
                         refs <- list_refs(args),
                         :ok <- validate_refs(refs, context.root_resource, {key, [left, value]}),
                         {:ok, function} <- Function.new(function_module, args) do
                      if is_nil(context.resource) ||
                           Ash.DataLayer.data_layer_can?(
                             context.resource,
                             {:filter_expr, function}
                           ) do
                        {:cont,
                         {:ok, BooleanExpression.optimized_new(:and, expression, function)}}
                      else
                        {:halt,
                         {:error, "data layer does not support the function #{inspect(function)}"}}
                      end
                    end
                end

              operator_module ->
                left =
                  if is_list(at_path) do
                    %Call{
                      name: :get_path,
                      args: [
                        %Ref{
                          attribute: attr,
                          relationship_path: context[:relationship_path] || [],
                          resource: context.resource,
                          input?: true
                        },
                        at_path
                      ]
                    }
                  else
                    %Ref{
                      attribute: attr,
                      relationship_path: context[:relationship_path] || [],
                      resource: context.resource,
                      input?: true
                    }
                  end

                with {:ok, [left, right]} <- hydrate_refs([left, value], context),
                     refs <- list_refs([left, right]),
                     :ok <- validate_refs(refs, context.root_resource, {attr, value}),
                     {:ok, operator} <- Operator.new(operator_module, left, right) do
                  if is_boolean(operator) do
                    {:cont, {:ok, operator}}
                  else
                    if is_nil(context.resource) ||
                         Ash.DataLayer.data_layer_can?(
                           context.resource,
                           {:filter_expr, operator}
                         ) do
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

  def get_predicate_function(key, resource, public?) do
    case get_function(key, resource, public?) |> List.wrap() |> Enum.filter(& &1.predicate?) do
      [] ->
        nil

      [function] ->
        function
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

    defp scrub_values(value) do
      Ash.Helpers.redact(value)
    end

    defp refers_to_sensitive?(expr) do
      Ash.Filter.find(expr, fn
        %Ref{attribute: %{sensitive?: true}} ->
          true

        _ ->
          false
      end)
    end
  end

  defp last_relationship(resource, list) do
    path = :lists.droplast(list)
    last = List.last(list)

    Ash.Resource.Info.relationship(Ash.Resource.Info.related(resource, path), last)
  end
end
