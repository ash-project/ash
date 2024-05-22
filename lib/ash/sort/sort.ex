defmodule Ash.Sort do
  @moduledoc """
  Utilities and types for sorting.

  ## Important

  Keyset pagination cannot currently be used in conjunction with aggregate and calculation sorting.
  Combining them will result in an error on the query.
  """

  @type sort_order ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last

  @type sort_item ::
          atom
          | {atom, sort_order}
          | %Ash.Query.Calculation{}
          | {%Ash.Query.Calculation{}, sort_order}
          | {atom, {Keyword.t() | map, sort_order}}

  @type t ::
          list(sort_item)
          | sort_item

  alias Ash.Error.Query.{InvalidSortOrder, NoSuchField}

  @doc """
  Builds an expression to be used in a sort statement.

  For example:

  ```elixir
  Ash.Query.sort(query, Ash.Sort.expr_sort(author.full_name, :string))

  Ash.Query.sort(query, [{Ash.Sort.expr_sort(author.full_name, :string), :desc_nils_first}])
  ```
  """
  @spec expr_sort(Ash.Expr.t(), Ash.Type.t() | nil) :: Ash.Expr.t()
  defmacro expr_sort(expression, type \\ nil) do
    quote generated: true do
      require Ash.Expr
      type = unquote(type)

      {type, constraints} =
        case type do
          {:array, _} ->
            {type, []}

          {type, constraints} ->
            {type, constraints}

          type ->
            {type, []}

          nil ->
            {nil, []}
        end

      type = type && Ash.Type.get_type(type)

      case Ash.Query.Calculation.new(
             :__expr_sort__,
             Ash.Resource.Calculation.Expression,
             [expr: Ash.Expr.expr(unquote(expression))],
             type,
             constraints
           ) do
        {:ok, calc} -> calc
        {:error, term} -> raise Ash.Error.to_ash_error(term)
      end
    end
  end

  @doc """
  A utility for parsing sorts provided from external input. Only allows sorting on public fields.

  The supported formats are:

  ### Sort Strings

  A comma separated list of fields to sort on, each with an optional prefix.

  The prefixes are:

  * "+" - Same as no prefix. Sorts `:asc`.
  * "++" - Sorts `:asc_nils_first`
  * "-" - Sorts `:desc`
  * "--" - Sorts `:desc_nils_last`

  For example

      "foo,-bar,++baz,--buz"

  ### A list of sort strings

  Same prefix rules as above, but provided as a list.

  For example:

      ["foo", "-bar", "++baz", "--buz"]


  ## Handling specific values

  A handler function may be provided that takes a string, and returns the relevant sort
  It won't be given any prefixes, only the field. This allows for things like parsing the calculation values
  out of the sort, or setting calculation values if they are not included in the sort string.

  To return calculation parameters, return `{:field, %{param: :value}}`. This will end up as something
  like `{:field, {%{param: :value}, :desc}}`, with the corresponding sort order.

  This handler function will only be called if you pass in a string or list of strings for the sort.
  Atoms will be assumed to have already been handled. The handler should return `nil` if it is not handling
  the given field.
  """
  @spec parse_input(
          Ash.Resource.t(),
          String.t()
          | list(atom | String.t() | {atom, sort_order()} | list(String.t()))
          | nil,
          nil | (String.t() -> nil | atom | {atom, map})
        ) ::
          {:ok, Ash.Sort.t()} | {:error, term}
  def parse_input(resource, sort, handler \\ nil)

  def parse_input(_, "", _), do: {:ok, []}

  def parse_input(resource, sort, handler) when is_binary(sort) do
    sort = String.split(sort, ",")
    parse_input(resource, sort, handler)
  end

  def parse_input(resource, sort, handler) when is_list(sort) do
    sort
    |> Enum.reduce_while({:ok, []}, fn field, {:ok, sort} ->
      case parse_sort(resource, field, handler) do
        {:ok, value} -> {:cont, {:ok, [value | sort]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      {:error, error} -> {:error, error}
    end
  end

  def parse_input(_resource, nil, _), do: {:ok, nil}

  @doc """
  Same as `parse_input/2` except raises any errors

  See `parse_input/2` for more.
  """
  def parse_input!(resource, sort, handler \\ nil) do
    case parse_input(resource, sort, handler) do
      {:ok, sort} ->
        sort

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  def parse_sort(resource, sort, handler \\ nil)

  def parse_sort(resource, {field, direction}, handler)
      when direction in [
             :asc,
             :desc,
             :asc_nils_first,
             :asc_nils_last,
             :desc_nils_first,
             :desc_nils_last
           ] do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, {field, direction}}
    end
  end

  def parse_sort(_resource, {_field, order}, _) do
    {:error, InvalidSortOrder.exception(order: order)}
  end

  def parse_sort(resource, "++" <> field, handler) do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, add_order(field, :asc_nils_first)}
    end
  end

  def parse_sort(resource, "--" <> field, handler) do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, add_order(field, :desc_nils_last)}
    end
  end

  def parse_sort(resource, "+" <> field, handler) do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, add_order(field, :asc)}
    end
  end

  def parse_sort(resource, "-" <> field, handler) do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, add_order(field, :desc)}
    end
  end

  def parse_sort(resource, field, handler) do
    case get_field(resource, field, handler) do
      nil -> {:error, NoSuchField.exception(resource: resource, field: field)}
      field -> {:ok, add_order(field, :asc)}
    end
  end

  defp add_order({field, map}, order) when is_map(map) do
    {field, {map, order}}
  end

  defp add_order(field, order) do
    {field, order}
  end

  defp get_field(resource, field, handler) do
    case call_handler(field, handler) do
      nil ->
        with nil <- Ash.Resource.Info.public_attribute(resource, field),
             nil <- Ash.Resource.Info.public_aggregate(resource, field),
             nil <- Ash.Resource.Info.public_calculation(resource, field) do
          nil
        else
          %{name: name} -> name
        end

      value ->
        value
    end
  end

  defp call_handler(field, handler) when is_binary(field) and is_function(handler, 1) do
    handler.(field)
  end

  defp call_handler(_, _), do: nil

  @doc """
  Reverses an Ash sort statement.
  """
  def reverse(sort) when is_list(sort) do
    Enum.map(sort, &reverse/1)
  end

  def reverse(sort) when is_atom(sort) do
    reverse({sort, :asc})
  end

  def reverse({key, {order, args}}) do
    {key, {reverse_order(order), args}}
  end

  def reverse({key, order}) do
    {key, reverse_order(order)}
  end

  defp reverse_order(:asc), do: :desc
  defp reverse_order(:desc), do: :asc
  defp reverse_order(:asc_nils_last), do: :desc_nils_first
  defp reverse_order(:asc_nils_first), do: :desc_nils_last
  defp reverse_order(:desc_nils_first), do: :asc_nils_last
  defp reverse_order(:desc_nils_last), do: :asc_nils_first

  @doc """
  A utility for sorting a list of records at runtime.

  For example:

      Ash.Sort.runtime_sort([record1, record2, record3], name: :asc, type: :desc_nils_last)

  Keep in mind that it is unrealistic to expect this runtime sort to always
  be exactly the same as a sort that may have been applied by your data layer.
  This is especially true for strings. For example, `Postgres` strings have a
  collation that affects their sorting, making it unpredictable from the perspective
  of a tool using the database: https://www.postgresql.org/docs/current/collation.html
  """
  defdelegate runtime_sort(results, sort, domain \\ nil), to: Ash.Actions.Sort
end
