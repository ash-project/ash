# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Combination do
  @moduledoc """
  Represents one combination in a combination of queries.
  """

  @type t :: %__MODULE__{
          filter: Ash.Expr.t(),
          sort: Ash.Sort.t(),
          limit: pos_integer() | nil,
          offset: pos_integer() | nil,
          select: [atom],
          calculations: %{atom() => Ash.Query.Calculation.t()},
          type: :base | :union | :union_all | :except | :intersect
        }

  defstruct [:filter, :sort, :limit, :offset, :select, calculations: %{}, type: :base]

  @doc """
  The initial combination of a combined query.
  """
  def base(opts) do
    %{struct(__MODULE__, opts) | type: :base}
  end

  @doc """
  Unions the query with the previous combinations, discarding duplicates when all fields are equal.
  """
  def union(opts) do
    %{struct(__MODULE__, opts) | type: :union}
  end

  @doc """
  Unions the query with the previous combinations, keeping all rows.
  """
  def union_all(opts) do
    %{struct(__MODULE__, opts) | type: :union_all}
  end

  @doc """
  Removes all rows that are present in the previous combinations *and* this one.
  """
  def except(opts) do
    %{struct(__MODULE__, opts) | type: :except}
  end

  @doc """
  Intersects the query with the previous combinations, keeping only rows that are present in the previous combinations and this one.
  """
  def intersect(opts) do
    %{struct(__MODULE__, opts) | type: :intersect}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(combination, opts) do
      if opts.custom_options[:in_query?] && combination.type != :base do
        sort? = combination.sort != []
        filter? = !!combination.filter
        limit? = !!combination.limit
        offset? = !!combination.offset
        select? = !!combination.select
        calculations? = !Enum.empty?(combination.calculations)

        container_doc(
          "#{combination.type} #Ash.Query.Combination<",
          [
            or_empty(concat("filter: ", to_doc(combination.filter, opts)), filter?),
            or_empty(concat("sort: ", to_doc(combination.sort, opts)), sort?),
            or_empty(concat("offset: ", to_doc(combination.offset, opts)), offset?),
            or_empty(concat("limit: ", to_doc(combination.limit, opts)), limit?),
            or_empty(concat("select: ", to_doc(combination.select, opts)), select?),
            or_empty(
              concat("calculations: ", to_doc(combination.calculations, opts)),
              calculations?
            )
          ],
          ">",
          opts,
          fn str, _ -> str end
        )
      else
        sort? = combination.sort != []
        filter? = !!combination.filter
        limit? = !!combination.limit
        offset? = !!combination.offset
        select? = !!combination.select
        calculations? = !Enum.empty?(combination.calculations)

        container_doc(
          "#Ash.Query.Combination<",
          [
            or_empty(concat("filter: ", to_doc(combination.filter, opts)), filter?),
            or_empty(concat("sort: ", to_doc(combination.sort, opts)), sort?),
            or_empty(concat("offset: ", to_doc(combination.offset, opts)), offset?),
            or_empty(concat("limit: ", to_doc(combination.limit, opts)), limit?),
            or_empty(concat("select: ", to_doc(combination.select, opts)), select?),
            or_empty(
              concat("calculations: ", to_doc(combination.calculations, opts)),
              calculations?
            )
          ],
          ">",
          opts,
          fn str, _ -> str end
        )
      end
    end

    defp or_empty(value, true), do: value
    defp or_empty(_, false), do: empty()
  end
end
