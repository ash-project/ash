defmodule Ash.Sort do
  @moduledoc """
  Utilities and types for sorting.

  ## Important

  Keyset pagination cannot currently be used in conjunction with aggregate and calculation sorting.
  Combining them will result in an error on the query.
  """

  @type sort_order ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last

  @type t :: list(atom | {atom, sort_order}) | atom

  alias Ash.Error.Query.{InvalidSortOrder, NoSuchAttribute}

  @doc """
  A utility for parsing sorts provided from external input. Only allows sorting
  on public attributes and aggregates.

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

  ### A standard Ash sort
  """
  @spec parse_input(
          Ash.Resource.t(),
          String.t()
          | list(atom | String.t() | {atom, sort_order()} | list(String.t()))
          | nil
        ) ::
          Ash.Sort.t() | nil
  def parse_input(resource, sort) when is_binary(sort) do
    sort = String.split(sort, ",")
    parse_input(resource, sort)
  end

  def parse_input(resource, sort) when is_list(sort) do
    sort
    |> Enum.reduce_while({:ok, []}, fn field, {:ok, sort} ->
      case parse_sort(resource, field) do
        {:ok, value} -> {:cont, {:ok, [value | sort]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      {:error, error} -> {:error, error}
    end
  end

  def parse_input(_resource, nil), do: nil

  def parse_sort(resource, {field, direction})
      when direction in [
             :asc,
             :desc,
             :asc_nils_first,
             :asc_nils_last,
             :desc_nils_first,
             :desc_nils_last
           ] do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, direction}}
    end
  end

  def parse_sort(_resource, {_field, order}) do
    {:error, InvalidSortOrder.exception(order: order)}
  end

  def parse_sort(resource, "++" <> field) do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, :asc_nils_first}}
    end
  end

  def parse_sort(resource, "--" <> field) do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, :desc_nils_last}}
    end
  end

  def parse_sort(resource, "+" <> field) do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, :asc}}
    end
  end

  def parse_sort(resource, "-" <> field) do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, :desc}}
    end
  end

  def parse_sort(resource, field) do
    case get_field(resource, field) do
      nil -> {:error, NoSuchAttribute.exception(resource: resource, name: field)}
      field -> {:ok, {field, :asc}}
    end
  end

  defp get_field(resource, field) do
    with nil <- Ash.Resource.Info.public_attribute(resource, field),
         nil <- Ash.Resource.Info.public_aggregate(resource, field),
         nil <- Ash.Resource.Info.public_calculation(resource, field) do
      nil
    else
      %{name: name} -> name
    end
  end

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
  defdelegate runtime_sort(results, sort), to: Ash.Actions.Sort
end
