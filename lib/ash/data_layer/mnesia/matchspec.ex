defmodule Ash.DataLayer.Mnesia.MatchSpec do
  @moduledoc """
  Converts an `Ash.Filter` to Mnesia matchspecs for efficient querying.

  ## Supported Operators

  * Equality: `Eq`, `NotEq`, `IsNil`
  * Comparison: `GreaterThan`, `GreaterThanOrEqual`, `LessThan`, `LessThanOrEqual`
  * Logical: `And`, `Or`, `Not`
  * Membership: `In`

  ## Limitations and room for improvement:

  * Right now we are building up a Guard and using wildcards for the MatchHead.
    This will be much faster than the [Runtime
    filters](lib/ash/filter/runtime.ex), but it could be improved upon by
    determining when to use a simple MatchHead if the filters are sufficiently
    simple.

  * We are always returning the full record (:"$_"). We could optimize for
    `select` queries by returning only the necessary fields.

  * There is no support for [Has queries](lib/ash/query/operator/has.ex).
  """

  @typedoc """
  A guard expression used in Mnesia matchspecs.
  Guards are tuples representing conditional expressions.

  The grammar for the MatchSpec Guards is available on the
  [erts matchspec page](https://www.erlang.org/doc/apps/erts/match_spec).
  """
  @type guard :: tuple() | atom() | boolean()

  @typedoc """
  The MatchSpec uses positional arguments to represent the fields being matched.
  This is a mapping of our `Ash.Resource` fields to matchspec variables (e.g.,
  :"$1", :"$2").
  """
  @type field_map :: %{atom() => atom()}

  @typedoc """
  A complete Mnesia matchspec in the format [{match_head, [guards], [result]}].

  For more information about the parts of a MatchSpec, see Mnesia's
  [select/3](https://www.erlang.org/doc/apps/mnesia/mnesia.html#select/3).
  """
  @type matchspec :: [{tuple(), [guard()], [term()]}]

  alias Ash.Query

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    IsNil,
    LessThan,
    LessThanOrEqual,
    NotEq
  }

  @doc """
  Converts an Ash.Filter to a complete Mnesia matchspec.

  Returns a matchspec in the format: [{match_head, [guards], [result]}]
  where the result is the full record (:"$_").

  ## Examples

      iex> filter = %Ash.Filter{resource: MyResource, expression: ...}
      iex> to_matchspec(filter)
      {:ok, [{match_head, [guards], [:"$_"]}]}

  """
  @spec to_matchspec(Ash.Filter.t() | nil) :: {:ok, matchspec()} | {:error, String.t()}
  def to_matchspec(%Ash.Filter{expression: expression, resource: resource}) do
    field_map = field_to_number_map(resource)
    match_head = build_match_head(resource)

    case parse(expression, field_map) do
      {:ok, guards} ->
        # Return the full record
        result = :"$_"
        {:ok, [{match_head, [guards], [result]}]}

      {:error, _} = error ->
        error
    end
  end

  def to_matchspec(nil) do
    {:ok, [{:_, [], [:"$_"]}]}
  end

  @spec field_to_number_map(Ash.Resource.t()) :: field_map()
  defp field_to_number_map(resource) do
    attribute_names = Ash.Resource.Info.attributes(resource) |> Enum.map(& &1.name)

    Ash.DataLayer.Mnesia.Info.table(resource)
    |> :mnesia.table_info(:wild_pattern)
    |> Tuple.to_list()
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn val, acc ->
      case val do
        # NOTE: Short of adding a large list of atoms in a module attribute, we
        # will get a Sobelow warning for this "unsafe" string to atom conversion
        {:_, index} -> Map.put(acc, Enum.at(attribute_names, index - 1), :"$#{index}")
        _ -> acc
      end
    end)
  end

  @spec build_match_head(Ash.Resource.t()) :: tuple()
  defp build_match_head(resource) do
    resource.to_ex_record(%{}, :position)
  end

  @doc """
  Parses an Ash.Filter or filter expression into a matchspec guard.

  ## Examples

      iex> filter = %Ash.Filter{expression: expr, resource: MyResource}
      iex> field_map = %{name: :"$1", age: :"$2"}
      iex> parse(filter, field_map)
      {:ok, guard}

  """
  @spec parse(term(), field_map()) :: {:ok, guard()} | {:error, String.t()}
  def parse(expression, field_map)

  def parse(%Query.BooleanExpression{op: :and, left: left, right: right} = _expression, field_map) do
    with {:ok, left_match} <- parse(left, field_map),
         {:ok, right_match} <- parse(right, field_map) do
      {:ok, {:andalso, left_match, right_match}}
    end
  end

  def parse(%Query.BooleanExpression{op: :or, left: left, right: right} = _expression, field_map) do
    with {:ok, left_match} <- parse(left, field_map),
         {:ok, right_match} <- parse(right, field_map) do
      {:ok, {:orelse, left_match, right_match}}
    end
  end

  def parse(%Query.Not{expression: expression}, field_map) do
    with {:ok, match} <- parse(expression, field_map) do
      {:ok, {:not, match}}
    end
  end

  def parse(%Eq{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:==, field_var, value}}
    end
  end

  def parse(%NotEq{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:"=/=", field_var, value}}
    end
  end

  def parse(%GreaterThan{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:>, field_var, value}}
    end
  end

  def parse(%GreaterThanOrEqual{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:>=, field_var, value}}
    end
  end

  def parse(%LessThan{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:<, field_var, value}}
    end
  end

  def parse(%LessThanOrEqual{left: field, right: value}, field_map) do
    with {:ok, field_var} <- get_field_var(field, field_map) do
      {:ok, {:<=, field_var, value}}
    end
  end

  def parse(%In{left: _field, right: []}, _field_map) do
    {:ok, false}
  end

  def parse(%In{left: field, right: values}, field_map) do
    with {:ok, guards} <- collect_results(values, field, field_map) do
      case guards do
        [] ->
          {:ok, false}

        [single] ->
          {:ok, single}

        [first | rest] ->
          result =
            Enum.reduce(rest, first, fn guard, acc ->
              {:orelse, acc, guard}
            end)

          {:ok, result}
      end
    end
  end

  def parse(%IsNil{left: left}, field_map) do
    parse(%Eq{left: left, right: nil}, field_map)
  end

  def parse(expression, _field_map) do
    {:error, "Unsupported filter expression: #{inspect(expression)}"}
  end

  # Used with the `In` parser to properly handle errors and return a list of
  # guards.
  defp collect_results(values, field, field_map) do
    values
    |> Enum.reduce_while({:ok, []}, fn value, {:ok, acc} ->
      case parse(%Eq{left: field, right: value}, field_map) do
        {:ok, guard} -> {:cont, {:ok, [guard | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, guards} -> {:ok, Enum.reverse(guards)}
      error -> error
    end
  end

  defp get_field_var(%Query.Ref{relationship_path: path}, _field_map)
       when path != [] do
    {:error, "Relationship traversal not supported in Mnesia matchspecs"}
  end

  defp get_field_var(field, field_map) do
    field_name = Query.Ref.name(field)

    case Map.fetch(field_map, field_name) do
      {:ok, var} ->
        {:ok, var}

      :error ->
        {:error, "Unknown field: #{field_name}"}
    end
  end
end
