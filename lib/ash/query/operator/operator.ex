defmodule Ash.Query.Operator do
  @moduledoc """
  An operator is a predicate with a `left` and a `right`

  For more information on being a predicate, see `Ash.Filter.Predicate`. Most of the complexities
  are there. An operator must meet both behaviours.
  """

  @doc """
  Create a new predicate. There are various return types possible:

    * `{:ok, left, right}` - Return the left/right values of the operator
    * `{:ok, operator}` - Return the operator itself, this or the one above are acceptable
    * `{:known, boolean}` - If the value is already known, e.g `1 == 1`
    * `{:error, error}` - If there was an error creating the operator
  """
  @callback new(term, term) ::
              {:ok, term, term} | {:ok, term} | {:known, boolean} | {:error, Ash.error()}

  @doc """
  The implementation of the inspect protocol.

  If not defined, it will be inferred
  """
  @callback to_string(struct, Inspect.Opts.t()) :: term

  alias Ash.Query.{Call, Expression, Not, Ref}

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
    case mod.new(left, right) do
      {:ok, val} ->
        case mod.evaluate(val) do
          {:known, value} -> {:ok, value}
          :unknown -> {:ok, val}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp try_cast_with_ref(mod, left, right) do
    Enum.find_value(mod.types(), fn type ->
      try_cast(left, right, type)
    end)
    |> case do
      nil ->
        {:error, "Could not cast expression"}

      {:ok, left, right} ->
        mod.new(left, right)
    end
  end

  defp try_cast(%Ref{attribute: %{type: type}} = left, right, :same) do
    case Ash.Type.cast_input(type, right) do
      {:ok, new_right} ->
        {:ok, left, new_right}

      _ ->
        nil
    end
  end

  defp try_cast(left, %Ref{attribute: %{type: type}} = right, :same) do
    case Ash.Type.cast_input(type, left) do
      {:ok, new_left} ->
        {:ok, new_left, right}

      _ ->
        nil
    end
  end

  defp try_cast(%Ref{attribute: %{type: type}} = left, right, [:any, {:array, :same}]) do
    case Ash.Type.cast_input({:array, type}, right) do
      {:ok, new_right} ->
        {:ok, left, new_right}

      _ ->
        nil
    end
  end

  defp try_cast(left, %Ref{attribute: %{type: {:array, type}}} = right, [:any, {:array, :same}]) do
    case Ash.Type.cast_input(type, left) do
      {:ok, new_left} ->
        {:ok, new_left, right}

      _ ->
        nil
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

  defp cast_one(value, type) do
    case Ash.Type.cast_input(type, value) do
      {:ok, casted} -> {:ok, casted}
      :error -> {:error, "Could not cast #{inspect(type)} as #{value}"}
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
    Enum.map(operators(), & &1.operator)
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

      alias Ash.Query.Ref

      def operator, do: unquote(opts[:operator])
      def name, do: unquote(opts[:name] || opts[:operator])

      def predicate? do
        unquote(opts[:predicate?])
      end

      def types do
        unquote(opts[:types] || [:same, :any])
      end

      def new(left, right), do: {:ok, struct(__MODULE__, left: left, right: right)}

      import Inspect.Algebra

      def to_string(%{left: left, right: right, operator: operator}, opts) do
        concat([
          to_doc(left, opts),
          " ",
          to_string(operator),
          " ",
          to_doc(right, opts)
        ])
      end

      defoverridable to_string: 2, new: 2

      defimpl Inspect do
        def inspect(%mod{} = op, opts) do
          mod.to_string(op, opts)
        end
      end
    end
  end
end
