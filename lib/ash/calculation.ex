defmodule Ash.Calculation do
  @moduledoc """
  The behaviour for a calculation module

  Use `select/2` to apply a select statement when the calculation is loaded.
  This does not apply in the case that you are loading on existing resources using
  `MyApi.load`. It also doesn't apply when the calculation is used in a filter or sort,
  because it is not necessary to select fields to power filters done in the data layer.
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Calculation

      @before_compile Ash.Calculation

      def init(opts), do: {:ok, opts}

      def describe(opts), do: "##{inspect(__MODULE__)}<#{inspect(opts)}>"

      def load(_query, _opts, _context), do: []

      def select(_query, _opts, _context), do: []

      defoverridable init: 1, describe: 1, select: 3, load: 3
    end
  end

  defmacro __before_compile__(_) do
    quote do
      if Module.defines?(__MODULE__, {:expression, 2}) do
        def has_expression?, do: true
      else
        def has_expression?, do: false
      end
    end
  end

  @type context :: %{
          :actor => term | nil,
          :tenant => term(),
          :authorize? => boolean | nil,
          :tracer => module | nil,
          optional(atom) => any
        }

  @type opts :: Keyword.t()

  @callback init(opts :: opts) :: {:ok, opts} | {:error, term}
  @callback describe(opts :: opts) :: String.t()
  @callback calculate(records :: [Ash.Resource.record()], opts :: opts, context :: context) ::
              {:ok, [term]} | [term] | {:error, term} | :unknown
  @callback expression(otps :: opts, context :: context) :: any
  @callback load(query :: Ash.Query.t(), opts :: opts, context :: context) ::
              atom | [atom] | Keyword.t()
  @callback select(query :: Ash.Query.t(), opts :: opts, context :: context) :: list(atom)
  @callback has_expression?() :: boolean()

  @optional_callbacks expression: 2, calculate: 3
end
