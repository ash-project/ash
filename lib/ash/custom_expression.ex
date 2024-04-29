defmodule Ash.CustomExpression do
  @moduledoc """
  A module for defining custom functions that can be called in Ash expressions.

  For example:

  ```elixir
  defmodule MyApp.Expressions.LevenshteinDistance do
    use Ash.CustomExpression,
      name: :levenshtein,
      arguments: [
        [:string, :string]
      ]

    def expression(AshPostgres.DataLayer, [left, right]) do
      {:ok, expr(fragment("levenshtein(?, ?)", left, right))}
    end

    # It is good practice to always define an expression for `Ash.DataLayer.Simple`,
    # as that is what Ash will use to run your custom expression in Elixir.
    # This allows us to completely avoid communicating with the database in some cases.

    def expression(data_layer, [left, right]) when data_layer in [
      Ash.DataLayer.Ets,
      Ash.DataLayer.Simple
    ] do
      {:ok, expr(fragment(&__MODULE__.levenshtein/2, left, right))}
    end

    # always define this fallback clause as well
    def expression(_data_layer, _args), do: :unknown

    @doc "Computes the levenshtein distance of two strings"
    def levenshtein(left, right) do
      # ......
    end
  end
  ```

  ### Options

  - `name` - The name of the custom expression. This is the name that will be used in Ash expressions.
  - `arguments` - A list of lists of types that the custom expression accepts. Each list represents a set of arguments that the custom expression can accept.
  - `predicate?` - Whether this expression can be exposed as a predicate in filter interfaces. Defaults to `false`.

  ### Registering Your Expression

  Use compile-time configuration to register your custom expressions

  ```elixir
  config :ash, :custom_expressions, [MyApp.Expressions.LevenshteinDistance]
  ```
  """

  defstruct [:module, :arguments, :expression, :simple_expression]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{expression: expression}, opts) do
      to_doc(expression, opts)
    end
  end

  @callback expression(
              data_layer :: Ash.DataLayer.t(),
              arguments :: list(Ash.Expr.t())
            ) ::
              {:ok, Ash.Expr.t()} | :unknown

  @callback name() :: atom

  @callback arguments() :: list(list(Ash.Type.t() | {Ash.Type.t(), Keyword.t()}))

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour Ash.CustomExpression

      import Ash.Expr

      if !opts[:name] do
        raise ArgumentError, "You must provide a name for the custom expression"
      end

      if !opts[:arguments] do
        raise ArgumentError, "You must provide arguments for the custom expression"
      end

      def predicate?, do: unquote(!!opts[:predicate?])
      def arguments, do: unquote(opts[:arguments])
      def name, do: unquote(opts[:name])
    end
  end
end
