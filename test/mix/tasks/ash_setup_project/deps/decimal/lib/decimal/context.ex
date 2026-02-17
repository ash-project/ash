defmodule Decimal.Context do
  import Decimal.Macros
  alias Decimal.Context

  @moduledoc """
  The context is kept in the process dictionary. It can be accessed with
  `get/0` and `set/1`.

  The default context has a precision of 28, the rounding algorithm is
  `:half_up`. The set trap enablers are `:invalid_operation` and
  `:division_by_zero`.

  ## Fields

    * `precision` - maximum number of decimal digits in the coefficient. If an
      operation result has more digits it will be rounded to `precision`
      digits with the rounding algorithm in `rounding`.
    * `rounding` - the rounding algorithm used when the coefficient's number of
      exceeds `precision`. Strategies explained below.
    * `flags` - a list of signals that for which the flag is sent. When an
      exceptional condition is signalled its flag is set. The flags are sticky
      and will be set until explicitly cleared.
    * `traps` - a list of set trap enablers for signals. When a signal's trap
      enabler is set the condition causes `Decimal.Error` to be raised.

  ## Rounding algorithms

    * `:down` - round toward zero (truncate). Discarded digits are ignored,
      result is unchanged.
    * `:half_up` - if the discarded digits is greater than or equal to half of
      the value of a one in the next left position then the coefficient will be
      incremented by one (rounded up). Otherwise (the discarded digits are less
      than half) the discarded digits will be ignored.
    * `:half_even` - also known as "round to nearest" or "banker's rounding". If
      the discarded digits is greater than half of the value of a one in the
      next left position then the coefficient will be incremented by one
      (rounded up). If they represent less than half discarded digits will be
      ignored. Otherwise (exactly half), the coefficient is not altered if it's
      even, or incremented by one (rounded up) if it's odd (to make an even
      number).
    * `:ceiling` - round toward +Infinity. If all of the discarded digits are
      zero or the sign is negative the result is unchanged. Otherwise, the
      coefficient will be incremented by one (rounded up).
    * `:floor` - round toward -Infinity. If all of the discarded digits are zero
      or the sign is positive the result is unchanged. Otherwise, the sign is
      negative and coefficient will be incremented by one.
    * `:half_down` - if the discarded digits is greater than half of the value
      of a one in the next left position then the coefficient will be
      incremented by one (rounded up). Otherwise (the discarded digits are half
      or less) the discarded digits are ignored.
    * `:up` - round away from zero. If all discarded digits are zero the
      coefficient is not changed, otherwise it is incremented by one (rounded
      up).

  This table shows the results of rounding operations for all the rounding
  algorithms:

  Rounding algorithm | 5.5 | 2.5 | 1.6 | 1.1 | 1.0 | -1.0 | -1.1 | -1.6 | -2.5 | -5.5
  :----------------- | :-- | :-- | :-- | :-- | :-- | :--- | :--- | :--- | :--- | :---
  `:up`              |   6 |   3 |   2 |   2 |   1 |   -1 |   -2 |   -2 |   -3 |   -6
  `:down`            |   5 |   2 |   1 |   1 |   1 |   -1 |   -1 |   -1 |   -2 |   -5
  `:ceiling`         |   6 |   3 |   2 |   2 |   1 |   -1 |   -1 |   -1 |   -2 |   -5
  `:floor`           |   5 |   2 |   1 |   1 |   1 |   -1 |   -2 |   -2 |   -3 |   -6
  `:half_up`         |   6 |   3 |   2 |   1 |   1 |   -1 |   -1 |   -2 |   -3 |   -6
  `:half_down`       |   5 |   2 |   2 |   1 |   1 |   -1 |   -1 |   -2 |   -2 |   -5
  `:half_even`       |   6 |   2 |   2 |   1 |   1 |   -1 |   -1 |   -2 |   -2 |   -6

  """
  @type t :: %__MODULE__{
          precision: pos_integer,
          rounding: Decimal.rounding(),
          flags: [Decimal.signal()],
          traps: [Decimal.signal()]
        }

  defstruct precision: 28,
            rounding: :half_up,
            flags: [],
            traps: [:invalid_operation, :division_by_zero]

  @context_key :"$decimal_context"

  @doc """
  Runs function with given context.
  """
  doc_since("1.9.0")
  @spec with(t(), (-> x)) :: x when x: var
  def with(%Context{} = context, fun) when is_function(fun, 0) do
    old = Process.put(@context_key, context)

    try do
      fun.()
    after
      set(old || %Context{})
    end
  end

  @doc """
  Gets the process' context.
  """
  doc_since("1.9.0")
  @spec get() :: t()
  def get() do
    Process.get(@context_key, %Context{})
  end

  @doc """
  Set the process' context.
  """
  doc_since("1.9.0")
  @spec set(t()) :: :ok
  def set(%Context{} = context) do
    Process.put(@context_key, context)
    :ok
  end

  @doc """
  Update the process' context.
  """
  doc_since("1.9.0")
  @spec update((t() -> t())) :: :ok
  def update(fun) when is_function(fun, 1) do
    get() |> fun.() |> set()
  end
end
