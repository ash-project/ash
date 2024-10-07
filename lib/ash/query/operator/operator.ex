defmodule Ash.Query.Operator do
  @moduledoc """
  An operator is a predicate with a `left` and a `right`

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. An operator must meet both behaviours.
  """

  alias Ash.Query.{Call, Expression, Not, Ref}

  @doc """
  Create a new predicate. There are various return types possible:

    * `{:ok, left, right}` - Return the left/right values of the operator
    * `{:ok, operator}` - Return the operator itself, this or the one above are acceptable
    * `{:known, boolean}` - If the value is already known, e.g `1 == 1`
    * `{:error, error}` - If there was an error creating the operator
  """
  @callback new(term, term) ::
              {:ok, term, term} | {:ok, term} | {:known, boolean} | {:error, term}

  @doc """
  The implementation of the inspect protocol.

  If not defined, it will be inferred
  """
  @callback to_string(struct, Inspect.Opts.t()) :: term

  @doc """
  Evaluates the operator in Elixir
  """
  @callback evaluate(term) :: term

  @doc """
  If `true`, will be allowed to evaluate `nil` inputs.

  If `false` (the default), any `nil` inputs will cause a `nil` return.
  """
  @callback evaluate_nil_inputs?() :: boolean()

  @doc """
  The types accepted by the operator. Defaults to `[:same, :any]`, which is any values of the same type.
  """
  @callback types() :: [
              :any | :same | [Ash.Type.t() | {Ash.Type.t(), constraints :: Keyword.t()}]
            ]

  @doc """
  The types that the expression can return. Should be one entry in the list for each entry in `types`.
  """
  @callback returns() :: [
              :any | :same | Ash.Type.t() | {Ash.Type.t(), constraints :: Keyword.t()}
            ]

  @callback predicate?() :: boolean()

  @doc """
  Whether or not the operator can evaluate to nil.
  """
  @callback can_return_nil?(func :: map) :: boolean()

  @doc "Evaluate the operator with provided inputs"
  def evaluate(%mod{left: left, right: right} = op) when is_nil(left) or is_nil(right) do
    if mod.evaluate_nil_inputs?() do
      do_evaluate(op)
    else
      {:known, nil}
    end
  end

  def evaluate(op) do
    do_evaluate(op)
  end

  defp do_evaluate(%mod{left: left, right: right} = op) do
    overloads = operator_overloads(mod.operator()) || []

    overloads
    |> Enum.find_value(fn {types, mod} ->
      case Ash.Type.determine_types([types], [left, right]) do
        [] ->
          nil

        _types ->
          if function_exported?(mod, :evaluate_operator, 1) do
            case mod.evaluate_operator(op) do
              {:known, value} ->
                {:known, value}

              _ ->
                nil
            end
          end
      end
    end)
    |> case do
      nil ->
        mod.evaluate(op)

      {:known, value} ->
        {:known, value}
    end
  end

  @doc "Create a new operator. Pass the module and the left and right values"
  def new(mod, %Ref{} = left, right) do
    try_cast_with_ref(mod, left, right)
  end

  def new(mod, left, %Ref{} = right) do
    try_cast_with_ref(mod, left, right)
  end

  def new(mod, %{__struct__: struct} = left, right)
      when struct in [Call, Expression, Not] do
    mod.new(left, right)
  end

  def new(mod, left, %{__struct__: struct} = right)
      when struct in [Call, Expression, Not] do
    mod.new(left, right)
  end

  def new(mod, %{__predicate__?: _} = left, right) do
    mod.new(left, right)
  end

  def new(mod, left, %{__predicate__?: _} = right) do
    mod.new(left, right)
  end

  def new(mod, left, right) do
    if Ash.Expr.expr?(left) or Ash.Expr.expr?(right) do
      mod.new(left, right)
    else
      case mod.new(left, right) do
        {:ok, val} ->
          case val do
            %mod{__predicate__?: _} ->
              case mod.evaluate(val) do
                {:known, value} -> {:ok, value}
                {:error, error} -> {:error, error}
                :unknown -> {:ok, val}
              end

            _ ->
              {:ok, val}
          end

        {:error, error} ->
          {:error, error}
      end
    end
  end

  @doc "Get type overloads for the given operator"
  def operator_overloads(operator) do
    :ash
    |> Application.get_env(:known_types, [])
    |> List.wrap()
    |> Enum.reduce(%{}, fn type, acc ->
      Code.ensure_compiled!(type)

      if function_exported?(type, :operator_overloads, 0) do
        Map.merge(acc, type.operator_overloads()[operator] || %{}, fn _, left, right ->
          Map.merge(left, right, fn _, left, right ->
            List.wrap(left) ++ List.wrap(right)
          end)
        end)
      else
        acc
      end
    end)
  end

  @doc false
  def try_cast_with_ref(mod, left, right) do
    Enum.find_value(mod.types(), fn type ->
      try_cast(left, right, type)
    end)
    |> case do
      nil ->
        {:error,
         Ash.Error.Query.InvalidFilterValue.exception(
           value: struct(mod, left: left, right: right),
           message: "No matching types. Possible types: #{inspect(mod.types())}"
         )}

      {:error, error} ->
        {:error, error}

      {:ok, left, right} ->
        mod.new(left, right)
    end
  end

  defp try_cast(%{__predicate__?: _} = left, right, _) do
    {:ok, left, right}
  end

  defp try_cast(left, %{__predicate__?: _} = right, _) do
    {:ok, left, right}
  end

  defp try_cast(%Ref{attribute: %{type: type, constraints: constraints}} = left, right, :same) do
    if Ash.Expr.expr?(right) do
      {:ok, left, right}
    else
      case Ash.Query.Type.try_cast(right, type, constraints) do
        {:ok, new_right} ->
          {:ok, left, new_right}

        _ ->
          nil
      end
    end
  end

  defp try_cast(left, %Ref{attribute: %{type: type, constraints: constraints}} = right, :same) do
    if Ash.Expr.expr?(right) do
      {:ok, left, right}
    else
      case Ash.Query.Type.try_cast(left, type, constraints) do
        {:ok, new_left} ->
          {:ok, new_left, right}

        _ ->
          nil
      end
    end
  end

  defp try_cast(%Ref{attribute: %{type: type, constraints: constraints}} = left, right, [
         :any,
         {:array, :same}
       ]) do
    if Ash.Expr.expr?(right) do
      {:ok, left, right}
    else
      case right do
        %Ref{attribute: %{type: {:array, _type}}} ->
          {:ok, left, right}

        %Ref{} ->
          nil

        right ->
          case Ash.Query.Type.try_cast(right, {:array, type}, items: constraints) do
            {:ok, new_right} ->
              {:ok, left, new_right}

            _ ->
              nil
          end
      end
    end
  end

  defp try_cast(
         left,
         %Ref{attribute: %{type: {:array, type}, constraints: constraints}} = right,
         [:any, {:array, :same}]
       ) do
    if Ash.Expr.expr?(right) do
      {:ok, left, right}
    else
      case Ash.Query.Type.try_cast(left, type, constraints) do
        {:ok, new_left} ->
          {:ok, new_left, right}

        _ ->
          nil
      end
    end
  end

  # We don't have a way to infer types from values right now
  defp try_cast(left, right, [:same, :same]) do
    {:ok, left, right}
  end

  defp try_cast(left, right, [:same, {:array, :same}]) do
    {:ok, left, right}
  end

  defp try_cast(left, right, [{:array, :same}, :same]) do
    {:ok, left, right}
  end

  defp try_cast(left, right, [{:array, :same}, {:array, :same}]) do
    {:ok, left, right}
  end

  defp try_cast(left, right, [:same, type]) do
    try_cast(left, right, [type, type])
  end

  defp try_cast(left, right, [type, :same]) do
    try_cast(left, right, [type, type])
  end

  defp try_cast(left, right, [{:array, :same}, type]) do
    try_cast(left, right, [{:array, type}, type])
  end

  defp try_cast(left, right, [type, :same]) do
    try_cast(left, right, [type, type])
  end

  defp try_cast(left, right, [type, {:array, :same}]) do
    try_cast(left, right, [type, {:array, type}])
  end

  defp try_cast(left, right, [left_type, right_type]) do
    with {:ok, left} <- cast_one(left, left_type),
         {:ok, right} <- cast_one(right, right_type) do
      {:ok, left, right}
    else
      {:error, error} ->
        {:error, error}
    end
  end

  defp try_cast(left, right, :any) do
    {:ok, left, right}
  end

  defp try_cast(left, right, {:array, :any}) do
    {:ok, left, right}
  end

  defp try_cast(_, _, _), do: nil

  defp cast_one(value, {:array, :any}) do
    {:ok, value}
  end

  defp cast_one(value, :any) do
    {:ok, value}
  end

  defp cast_one(value, {type, constraints}) do
    if Ash.Expr.expr?(value) do
      {:ok, value}
    else
      case Ash.Query.Type.try_cast(value, type, constraints) do
        {:ok, casted} ->
          {:ok, casted}

        _ ->
          {:error, "Could not cast #{inspect(value)} as #{inspect(type)}"}
      end
    end
  end

  defp cast_one(value, type) do
    if Ash.Expr.expr?(value) do
      {:ok, value}
    else
      case Ash.Query.Type.try_cast(value, type) do
        {:ok, casted} ->
          {:ok, casted}

        _ ->
          {:error, "Could not cast #{inspect(value)} as #{inspect(type)}"}
      end
    end
  end

  def operators do
    [
      Ash.Query.Operator.Eq,
      Ash.Query.Operator.GreaterThanOrEqual,
      Ash.Query.Operator.GreaterThan,
      Ash.Query.Operator.In,
      Ash.Query.Operator.IsNil,
      Ash.Query.Operator.LessThanOrEqual,
      Ash.Query.Operator.LessThan,
      Ash.Query.Operator.NotEq
    ] ++ Ash.Query.Operator.Basic.operator_modules()
  end

  def operator_symbols do
    Enum.map(operators(), & &1.operator())
  end

  defmacro __using__(opts) do
    unless opts[:operator] do
      raise "Operator is required!"
    end

    quote do
      defstruct [
        :left,
        :right,
        operator: unquote(opts[:operator]),
        embedded?: false,
        __operator__?: true,
        __predicate__?: unquote(opts[:predicate?] || false)
      ]

      if unquote(opts[:predicate?]) do
        @behaviour Ash.Filter.Predicate
      end

      @behaviour Ash.Query.Operator

      alias Ash.Query.Ref
      import Inspect.Algebra

      def operator, do: unquote(opts[:operator])
      def name, do: unquote(opts[:name] || opts[:operator])

      @impl Ash.Query.Operator
      def predicate?, do: unquote(opts[:predicate?] || false)

      @impl Ash.Query.Operator
      if unquote(opts[:predicate?]) do
        def returns do
          Enum.map(List.wrap(unquote(opts[:types])), fn _ -> :boolean end)
        end
      else
        def returns do
          unquote(opts[:returns] || :unknown)
        end
      end

      @impl Ash.Query.Operator
      def types do
        unquote(opts[:types] || [:same, :any])
      end

      @impl Ash.Query.Operator
      def new(left, right), do: {:ok, struct(__MODULE__, left: left, right: right)}

      @impl Ash.Query.Operator
      def evaluate_nil_inputs?, do: false

      @impl Ash.Query.Operator
      def to_string(%{left: left, right: right, operator: operator}, opts) do
        concat([
          to_doc(left, opts),
          " ",
          to_string(operator),
          " ",
          to_doc(right, opts)
        ])
      end

      @impl Ash.Query.Operator
      def can_return_nil?(_), do: true

      defoverridable to_string: 2, new: 2, evaluate_nil_inputs?: 0, can_return_nil?: 1

      defimpl Inspect do
        def inspect(%mod{} = op, opts) do
          mod.to_string(op, opts)
        end
      end
    end
  end
end
