defmodule Ash.Filter.TemplateHelpers do
  @moduledoc "Helpers for building filter templates"

  @doc "A helper for using actor values in filter templates"
  def actor(value), do: {:_actor, value}

  @doc "A helper for using action arguments in filter templates"
  def arg(name), do: {:_arg, name}

  def tenant(), do: {:_tenant}

  @doc """
  A helper for using query context in filter templates

  An atom will just get the key, and a list will be accessed via `get_in`.
  """
  def context(name), do: {:_context, name}

  @doc "A helper for building an expression style filter"
  defmacro expr(expr) do
    quote do
      require Ash.Query

      Ash.Query.expr(unquote(expr))
    end
  end
end
