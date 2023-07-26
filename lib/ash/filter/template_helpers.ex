defmodule Ash.Filter.TemplateHelpers do
  @moduledoc "Helpers for building filter templates"

  @deprecated "Use `expr?/1` instead, which is not a guard"
  defguard is_expr(value)
           when is_struct(value, Ash.Query.Not) or is_struct(value, Ash.Query.BooleanExpression) or
                  is_struct(value, Ash.Query.Call) or is_struct(value, Ash.Query.Ref) or
                  is_struct(value, Ash.Query.Exists) or
                  is_struct(value, Ash.Query.Parent) or
                  (is_struct(value) and is_map_key(value, :__predicate__?))

  def expr?(value)
      when is_struct(value, Ash.Query.Not) or is_struct(value, Ash.Query.BooleanExpression) or
             is_struct(value, Ash.Query.Call) or is_struct(value, Ash.Query.Ref) or
             is_struct(value, Ash.Query.Exists) or
             is_struct(value, Ash.Query.Parent) or
             (is_struct(value) and is_map_key(value, :__predicate__?)) do
    true
  end

  def expr?(value) when is_list(value) do
    Enum.any?(value, &expr?/1)
  end

  def expr?(value) when is_map(value) do
    value
    |> Map.keys()
    |> Enum.any?(&expr?/1)
  end

  def expr?({left, right}) do
    expr?(left) || expr?(right)
  end

  def expr?(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> expr?()
  end

  def expr?(_), do: false

  @doc "A helper for using actor values in filter templates"
  def actor(value), do: {:_actor, value}

  @doc "A helper for using action arguments in filter templates"
  def arg(name), do: {:_arg, name}

  @doc "A helper for creating a reference"
  def ref(name), do: {:_ref, [], name}

  @doc "A helper for creating a parent reference"
  def parent(expr), do: {:_parent, [], expr}

  @doc "A helper for creating a reference to a related path"
  def ref(path, name), do: {:_ref, path, name}

  @doc """
  A helper for using query context in filter templates

  An atom will just get the key, and a list will be accessed via `get_in`.
  """
  def context(name), do: {:_context, name}

  @doc "A helper for building an expression style filter"
  defmacro expr(expr) do
    quote do
      require Ash.Expr

      Ash.Expr.expr(unquote(expr))
    end
  end
end
