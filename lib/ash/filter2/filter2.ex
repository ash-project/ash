defmodule Ash.Filter2 do
  alias Ash.Filter2.Predicate
  alias Ash.Filter2.Predicate.{Eq, In}
  alias Ash.Filter2.Expression
  alias Ash.Filter2.Not

  @built_in_predicates [
    eq: Eq,
    in: In
  ]

  defstruct [:resource, :api, :expression]

  def parse!(api, resource, statement) do
    case parse(api, resource, statement) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise error
    end
  end

  def parse(api, resource, statement) do
    context = %{
      resource: resource,
      api: api,
      relationship_path: []
    }

    case parse_expression(statement, context) do
      {:ok, expression} ->
        {:ok, %__MODULE__{expression: expression, resource: resource, api: api}}

      {:error, error} ->
        error = Ash.Error.to_ash_error(error)
        {:error, error}
    end
  end

  def parse_expression(statement, context) do
    statement
    |> List.wrap()
    |> Enum.reduce_while({:ok, nil}, fn expression_part, {:ok, expression} ->
      case expression_part do
        {:not, nested_statement} ->
          case parse_expression(nested_statement, context) do
            {:ok, nested_expression} ->
              {:cont, {:ok, Expression.new(:and, expression, Not.new(nested_expression))}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {op, nested_statements} when op in [:or, :and] ->
          case parse_and_join(nested_statements, op, context) do
            {:ok, nested_expression} ->
              {:cont, {:ok, Expression.new(:and, expression, nested_expression)}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {field, nested_statement} when is_atom(field) or is_binary(field) ->
          cond do
            attr = Ash.attribute(context.resource, field) ->
              case parse_predicates(nested_statement, attr, context) do
                {:ok, nested_statement} ->
                  {:cont, {:ok, Expression.new(:and, expression, nested_statement)}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end

            rel = Ash.relationship(context.resource, field) ->
              context =
                context
                |> Map.update!(:relationship_path, fn path -> path ++ [rel.name] end)
                |> Map.put(:resource, rel.destination)

              case parse_expression(nested_statement, context) do
                {:ok, nested_statement} ->
                  {:cont, {:ok, Expression.new(:and, expression, nested_statement)}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end

            true ->
              {:halt,
               {:error,
                "No such attribute or relationship #{field} on #{inspect(context.resource)}"}}
          end
      end
    end)
  end

  defp parse_and_join(statements, op, context) do
    Enum.reduce_while(statements, {:ok, nil}, fn statement, {:ok, expression} ->
      case parse_expression(statement, context) do
        {:ok, nested_expression} ->
          {:cont, {:ok, Expression.new(op, expression, nested_expression)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_predicates(value, field, context) when not is_list(value) do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(values, attr, context) do
    if Keyword.keyword?(values) do
      Enum.reduce_while(values, {:ok, nil}, fn {key, value}, {:ok, expression} ->
        case @built_in_predicates[key] do
          nil ->
            {:halt, {:error, "No such filter predicate: #{inspect(key)}"}}

          predicate_module ->
            case Predicate.new(
                   context.resource,
                   attr,
                   predicate_module,
                   value,
                   context.relationship_path
                 ) do
              {:ok, predicate} ->
                {:cont, {:ok, Expression.new(:and, expression, predicate)}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
        end
      end)
    else
      {:halt, {:error, "Invalid filter expression: #{inspect(values)}"}}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(
          %{expression: expression},
          opts
        ) do
      concat(["#Ash.Filter<", to_doc(expression, opts), ">"])
    end
  end
end
