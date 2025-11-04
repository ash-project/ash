# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Sort do
  @moduledoc """
  Utilities and types for sorting.
  """

  @type sort_order ::
          :asc | :desc | :asc_nils_first | :asc_nils_last | :desc_nils_first | :desc_nils_last

  @type sort_item ::
          String.t()
          | atom
          | {atom, sort_order}
          | %Ash.Query.Calculation{}
          | {%Ash.Query.Calculation{}, sort_order}
          | {atom, {Keyword.t() | map, sort_order}}

  @type t ::
          list(sort_item)
          | sort_item

  alias Ash.Error.Query.{InvalidSortOrder, NoSuchField, UnsortableField}

  @doc """
  Builds an expression to be used in a sort statement. Prefer to use `Ash.Expr.calc/2` instead.

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

      Ash.Expr.calc(unquote(expression),
        type: type,
        constraints: constraints,
        name: :__calc__
      )
    end
  end

  @doc """
  A utility for parsing sorts provided from external input. Only allows sorting on public fields.

  See `Ash.Query.sort/3` for supported formats.

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

  def parse_input(_, [], _), do: {:ok, []}

  def parse_input(resource, sort, handler) when is_binary(sort) do
    sort = String.split(sort, ",")
    parse_input(resource, sort, handler)
  end

  def parse_input(resource, field, handler) when is_atom(field) do
    case parse_sort(resource, field, handler) do
      {:ok, value} -> {:ok, [value]}
      {:error, error} -> {:error, error}
    end
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

  def parse_sort(resource, sort, handler \\ nil, public_only? \\ true)

  def parse_sort(resource, {field, {input, direction}}, handler, public_only?)
      when direction in [
             :asc,
             :desc,
             :asc_nils_first,
             :asc_nils_last,
             :desc_nils_first,
             :desc_nils_last
           ] do
    case get_field(resource, field, handler, public_only?, input) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, direction)}
    end
  end

  def parse_sort(resource, {field, direction}, handler, public_only?)
      when direction in [
             :asc,
             :desc,
             :asc_nils_first,
             :asc_nils_last,
             :desc_nils_first,
             :desc_nils_last
           ] do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, direction)}
    end
  end

  def parse_sort(resource, {field, input}, handler, public_only?) when not is_atom(input) do
    case get_field(resource, field, handler, public_only?, input) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :asc)}
    end
  end

  def parse_sort(_resource, {_field, order}, _, _) do
    {:error, InvalidSortOrder.exception(order: order)}
  end

  def parse_sort(resource, "++" <> field, handler, public_only?) do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :asc_nils_first)}
    end
  end

  def parse_sort(resource, "--" <> field, handler, public_only?) do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :desc_nils_last)}
    end
  end

  def parse_sort(resource, "+" <> field, handler, public_only?) do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :asc)}
    end
  end

  def parse_sort(resource, "-" <> field, handler, public_only?) do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :desc)}
    end
  end

  def parse_sort(resource, field, handler, public_only?) do
    case get_field(resource, field, handler, public_only?) do
      {:error, error} -> {:error, error}
      {:ok, field} -> {:ok, add_order(field, :asc)}
    end
  end

  defp add_order({field, map}, order) when is_map(map) do
    {field, {map, order}}
  end

  defp add_order(field, order) do
    {field, order}
  end

  defp get_field(resource, field, handler, only_public?, input \\ %{}) do
    case call_handler(field, handler) do
      nil ->
        {path, field} =
          cond do
            is_binary(field) ->
              case Enum.reverse(String.split(field, ".", trim: true)) do
                [] -> {[], ""}
                [field | path] -> {Enum.reverse(path), field}
              end

            is_atom(field) ->
              case Enum.reverse(String.split(to_string(field), ".", trim: true)) do
                [] -> {[], ""}
                [_] -> {[], field}
                [field | path] -> {Enum.reverse(path), field}
              end

            true ->
              {[], field}
          end

        case path do
          [] ->
            case do_get_field(resource, field, only_public?, input) do
              {:ok, field} ->
                if type_sortable?(resource, field) do
                  case field do
                    %Ash.Query.Aggregate{} = field -> {:ok, field}
                    %Ash.Query.Calculation{} = field -> {:ok, field}
                    %{name: name} -> {:ok, name}
                  end
                else
                  {:error, UnsortableField.exception(name: field.name, resource: resource)}
                end

              {:error, error} ->
                {:error, error}
            end

          path ->
            case related_field(resource, path, field, only_public?, input) do
              {:ok, path, field, type, constraints} ->
                case field do
                  %struct{} = thing when struct in [Ash.Query.Aggregate, Ash.Query.Calculation] ->
                    ref = %Ash.Query.Ref{
                      attribute: thing,
                      relationship_path: path,
                      resource: Ash.Resource.Info.related(resource, path)
                    }

                    Ash.Query.Calculation.new(
                      :__calc__,
                      Ash.Resource.Calculation.Expression,
                      [expr: ref],
                      type,
                      constraints
                    )

                  %{name: name} ->
                    ref = %Ash.Query.Ref{
                      attribute: name,
                      relationship_path: path,
                      resource: Ash.Resource.Info.related(resource, path)
                    }

                    Ash.Query.Calculation.new(
                      :__calc__,
                      Ash.Resource.Calculation.Expression,
                      [expr: ref],
                      type,
                      constraints
                    )
                end

              {:error, error} ->
                {:error, error}
            end
        end

      {:error, error} ->
        {:error, error}

      {:ok, ok} ->
        {:ok, ok}

      value ->
        {:ok, value}
    end
  end

  defp do_get_field(resource, field, true, input) do
    case Ash.Resource.Info.public_field(resource, field) do
      %Ash.Resource.Calculation{} = calc ->
        with {:ok, calc} <-
               Ash.Query.Calculation.from_resource_calculation(resource, calc, args: input) do
          {:ok, %{calc | name: :__calc__}}
        end

      nil ->
        {:error, NoSuchField.exception(field: field, resource: resource)}

      other ->
        {:ok, other}
    end
  end

  defp do_get_field(_resource, %Ash.Query.Calculation{} = calc, _, _input) do
    {:ok, calc}
  end

  defp do_get_field(_resource, %Ash.Query.Aggregate{} = agg, _, _input) do
    {:ok, agg}
  end

  defp do_get_field(resource, field, _, input) do
    case Ash.Resource.Info.field(resource, field) do
      %Ash.Resource.Calculation{} = calc ->
        with {:ok, calc} <-
               Ash.Query.Calculation.from_resource_calculation(resource, calc, args: input) do
          {:ok, %{calc | name: :__calc__}}
        end

      nil ->
        {:error, NoSuchField.exception(field: field, resource: resource)}

      other ->
        {:ok, other}
    end
  end

  defp do_get_relationship(resource, relationship, true) do
    Ash.Resource.Info.public_relationship(resource, relationship)
  end

  defp do_get_relationship(resource, relationship, _) do
    Ash.Resource.Info.relationship(resource, relationship)
  end

  defp type_sortable?(resource, %Ash.Resource.Aggregate{} = aggregate) do
    case aggregate_type(resource, aggregate) do
      {type, constraints} ->
        do_type_sortable?(resource, type, constraints)

      nil ->
        false
    end
  end

  # an ugly workaround
  defp type_sortable?(_resource, %Ash.Query.Calculation{}), do: true

  defp type_sortable?(resource, field) do
    do_type_sortable?(resource, field.type, field.constraints)
  end

  defp aggregate_type(resource, aggregate) do
    attribute =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.field(related, aggregate.field)
      end

    case attribute do
      %Ash.Resource.Aggregate{} = related_aggregate ->
        case aggregate_type(
               Ash.Resource.Info.related(resource, aggregate.relationship_path),
               related_aggregate
             ) do
          {type, constraints} ->
            case Ash.Query.Aggregate.kind_to_type(aggregate.kind, type, constraints) do
              {:ok, type, constraints} ->
                {type, constraints}

              _ ->
                nil
            end

          nil ->
            nil
        end

      nil ->
        case Ash.Query.Aggregate.kind_to_type(aggregate.kind, nil, nil) do
          {:ok, type, constraints} ->
            {type, constraints}

          _ ->
            nil
        end

      field ->
        case Ash.Query.Aggregate.kind_to_type(aggregate.kind, field.type, field.constraints) do
          {:ok, type, constraints} ->
            {type, constraints}

          _ ->
            nil
        end
    end
  end

  defp get_type(resource, %Ash.Resource.Aggregate{} = aggregate) do
    attribute =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field)
      end

    attribute_type =
      if attribute do
        attribute.type
      end

    attribute_constraints =
      if attribute do
        attribute.constraints
      end

    case Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type, attribute_constraints) do
      {:ok, type, constraints} ->
        {type, constraints}

      _other ->
        nil
    end
  end

  defp get_type(_resource, field) do
    {field.type, field.constraints}
  end

  defp do_type_sortable?(resource, type, constraints) do
    if Ash.Type.embedded_type?(type) do
      false
    else
      Ash.DataLayer.data_layer_can?(
        resource,
        {:sort, Ash.Type.storage_type(type, constraints)}
      )
    end
  end

  defp call_handler(field, handler) when is_binary(field) and is_function(handler, 1) do
    handler.(field)
  end

  defp call_handler(_, _), do: nil

  defp related_field(resource, path, field, only_public?, input, acc \\ [])

  defp related_field(resource, [], field, only_public?, input, acc) do
    case do_get_field(resource, field, only_public?, input) do
      {:error, error} ->
        {:error, error}

      {:ok, nil} ->
        {:error, NoSuchField.exception(field: field, resource: resource)}

      {:ok, %{sortable?: true} = field} ->
        {type, constraints} = get_type(resource, field)

        if do_type_sortable?(resource, type, constraints) do
          {:ok, Enum.reverse(acc), field, type, constraints}
        else
          {:error, UnsortableField.exception(name: field.name, resource: resource)}
        end

      {:ok, %{name: name}} ->
        {:error, UnsortableField.exception(name: name, resource: resource)}
    end
  end

  defp related_field(resource, [first | rest], field, only_public?, input, acc) do
    case do_get_relationship(resource, first, only_public?) do
      %{sortable?: false, name: name} ->
        {:error, UnsortableField.exception(name: name, resource: resource)}

      %{sortable?: true, destination: destination, name: name} ->
        related_field(destination, rest, field, only_public?, input, [name | acc])

      _ ->
        {:error, NoSuchField.exception(field: field, resource: resource)}
    end
  end

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
  defdelegate runtime_sort(results, sort, opts \\ []), to: Ash.Actions.Sort
end
