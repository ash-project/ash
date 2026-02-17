defmodule Decimal do
  @moduledoc """
  Decimal arithmetic on arbitrary precision floating-point numbers.

  A number is represented by a signed coefficient and exponent such that: `sign
  * coefficient * 10 ^ exponent`. All numbers are represented and calculated
  exactly, but the result of an operation may be rounded depending on the
  context the operation is performed with, see: `Decimal.Context`. Trailing
  zeros in the coefficient are never truncated to preserve the number of
  significant digits unless explicitly done so.

  There are also special values such as NaN (not a number) and ±Infinity.
  -0 and +0 are two distinct values.
  Some operation results are not defined and will return NaN.
  This kind of NaN is quiet, any operation returning a number will return
  NaN when given a quiet NaN (the NaN value will flow through all operations).

  Exceptional conditions are grouped into signals, each signal has a flag and a
  trap enabler in the context. Whenever a signal is triggered it's flag is set
  in the context and will be set until explicitly cleared. If the signal is trap
  enabled `Decimal.Error` will be raised.

  ## Specifications

    * [IBM's General Decimal Arithmetic Specification](http://speleotrove.com/decimal/decarith.html)
    * [IEEE standard 854-1987](http://web.archive.org/web/20150908012941/http://754r.ucbtest.org/standards/854.pdf)

  This library follows the above specifications for reference of arithmetic
  operation implementations, but the public APIs may differ to provide a
  more idiomatic Elixir interface.

  The specification models the sign of the number as 1, for a negative number,
  and 0 for a positive number. Internally this implementation models the sign as
  1 or -1 such that the complete number will be `sign * coefficient *
  10 ^ exponent` and will refer to the sign in documentation as either *positive*
  or *negative*.

  There is currently no maximum or minimum values for the exponent. Because of
  that all numbers are "normal". This means that when an operation should,
  according to the specification, return a number that "underflows" 0 is returned
  instead of Etiny. This may happen when dividing a number with infinity.
  Additionally, overflow, underflow and clamped may never be signalled.
  """

  import Bitwise
  import Kernel, except: [abs: 1, div: 2, max: 2, min: 2, rem: 2, round: 1]
  import Decimal.Macros
  alias Decimal.Context
  alias Decimal.Error

  @power_of_2_to_52 4_503_599_627_370_496

  @typedoc """
  The coefficient of the power of `10`. Non-negative because the sign is stored separately in `sign`.

    * `non_neg_integer` - when the `t` represents a number, instead of one of the special values below.
    * `:NaN` - Not a Number.
    * `:inf` - Infinity.

  """
  @type coefficient :: non_neg_integer | :NaN | :inf

  @typedoc """
  The exponent to which `10` is raised.
  """
  @type exponent :: integer

  @typedoc """

    * `1` for positive
    * `-1` for negative

  """
  @type sign :: 1 | -1

  @type signal ::
          :invalid_operation
          | :division_by_zero
          | :rounded
          | :inexact

  @type compare_result ::
          :lt | :gt | :eq

  @typedoc """
  Rounding algorithm.

  See `Decimal.Context` for more information.
  """
  @type rounding ::
          :down
          | :half_up
          | :half_even
          | :ceiling
          | :floor
          | :half_down
          | :up

  @typedoc """
  This implementation models the `sign` as `1` or `-1` such that the complete number will be: `sign * coef * 10 ^ exp`.

    * `coef` - the coefficient of the power of `10`.
    * `exp` - the exponent of the power of `10`.
    * `sign` - `1` for positive, `-1` for negative.

  """
  @type t :: %__MODULE__{
          sign: sign,
          coef: coefficient,
          exp: exponent
        }

  @type decimal :: t | integer | String.t()

  defstruct sign: 1, coef: 0, exp: 0

  defmacrop error(flags, reason, result, context \\ nil) do
    quote bind_quoted: binding() do
      case handle_error(flags, reason, result, context) do
        {:ok, result} -> result
        {:error, error} -> raise Error, error
      end
    end
  end

  @doc """
  Returns `true` if number is NaN, otherwise `false`.

  ## Examples

      iex> Decimal.nan?(Decimal.new("NaN"))
      true

      iex> Decimal.nan?(Decimal.new(42))
      false

  """
  @spec nan?(t) :: boolean
  def nan?(%Decimal{coef: :NaN}), do: true
  def nan?(%Decimal{}), do: false

  @doc """
  Returns `true` if number is ±Infinity, otherwise `false`.

  ## Examples

      iex> Decimal.inf?(Decimal.new("+Infinity"))
      true

      iex> Decimal.inf?(Decimal.new("-Infinity"))
      true

      iex> Decimal.inf?(Decimal.new("1.5"))
      false

  """
  @spec inf?(t) :: boolean
  def inf?(%Decimal{coef: :inf}), do: true
  def inf?(%Decimal{}), do: false

  @doc """
  Returns `true` if argument is a decimal number, otherwise `false`.

  ## Examples

      iex> Decimal.is_decimal(Decimal.new(42))
      true

      iex> Decimal.is_decimal(42)
      false

  Allowed in guard tests on OTP 21+.
  """
  doc_since("1.9.0")
  defmacro is_decimal(term)

  if function_exported?(:erlang, :is_map_key, 2) do
    defmacro is_decimal(term) do
      case __CALLER__.context do
        nil ->
          quote do
            case unquote(term) do
              %Decimal{} -> true
              _ -> false
            end
          end

        :match ->
          raise ArgumentError,
                "invalid expression in match, is_decimal is not allowed in patterns " <>
                  "such as function clauses, case clauses or on the left side of the = operator"

        :guard ->
          quote do
            is_map(unquote(term)) and :erlang.is_map_key(:__struct__, unquote(term)) and
              :erlang.map_get(:__struct__, unquote(term)) == Decimal
          end
      end
    end
  else
    # TODO: remove when we require Elixir v1.10
    defmacro is_decimal(term) do
      quote do
        case unquote(term) do
          %Decimal{} -> true
          _ -> false
        end
      end
    end
  end

  @doc """
  The absolute value of given number. Sets the number's sign to positive.

  ## Examples

      iex> Decimal.abs(Decimal.new("1"))
      Decimal.new("1")

      iex> Decimal.abs(Decimal.new("-1"))
      Decimal.new("1")

      iex> Decimal.abs(Decimal.new("NaN"))
      Decimal.new("NaN")

  """
  @spec abs(t) :: t
  def abs(%Decimal{coef: :NaN} = num), do: %{num | sign: 1}
  def abs(%Decimal{} = num), do: context(%{num | sign: 1})

  @doc """
  Adds two numbers together.

  ## Exceptional conditions

    * If one number is -Infinity and the other +Infinity, `:invalid_operation` will
      be signalled.

  ## Examples

      iex> Decimal.add(1, "1.1")
      Decimal.new("2.1")

      iex> Decimal.add(1, "Inf")
      Decimal.new("Infinity")

  """
  @spec add(decimal, decimal) :: t
  def add(%Decimal{coef: :NaN} = num1, %Decimal{}), do: num1

  def add(%Decimal{}, %Decimal{coef: :NaN} = num2), do: num2

  def add(%Decimal{coef: :inf, sign: sign} = num1, %Decimal{coef: :inf, sign: sign} = num2) do
    if num1.exp > num2.exp do
      num1
    else
      num2
    end
  end

  def add(%Decimal{coef: :inf}, %Decimal{coef: :inf}),
    do: error(:invalid_operation, "adding +Infinity and -Infinity", %Decimal{coef: :NaN})

  def add(%Decimal{coef: :inf} = num1, %Decimal{}), do: num1

  def add(%Decimal{}, %Decimal{coef: :inf} = num2), do: num2

  def add(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2

    {coef1, coef2} = add_align(coef1, exp1, coef2, exp2)
    coef = sign1 * coef1 + sign2 * coef2
    exp = Kernel.min(exp1, exp2)
    sign = add_sign(sign1, sign2, coef)
    context(%Decimal{sign: sign, coef: Kernel.abs(coef), exp: exp})
  end

  def add(num1, num2), do: add(decimal(num1), decimal(num2))

  @doc """
  Subtracts second number from the first. Equivalent to `Decimal.add/2` when the
  second number's sign is negated.

  ## Exceptional conditions

    * If one number is -Infinity and the other +Infinity `:invalid_operation` will
      be signalled.

  ## Examples

      iex> Decimal.sub(1, "0.1")
      Decimal.new("0.9")

      iex> Decimal.sub(1, "Inf")
      Decimal.new("-Infinity")

  """
  @spec sub(decimal, decimal) :: t
  def sub(%Decimal{} = num1, %Decimal{sign: sign} = num2) do
    add(num1, %{num2 | sign: -sign})
  end

  def sub(num1, num2) do
    sub(decimal(num1), decimal(num2))
  end

  @doc """
  Compares two numbers numerically using a threshold. If the first number added
  to the threshold is greater than the second number, and the first number
  subtracted by the threshold is smaller than the second number, then the two
  numbers are considered equal.

  ## Examples

      iex> Decimal.compare("1.1", 1, "0.2")
      :eq

      iex> Decimal.compare("1.2", 1, "0.1")
      :gt

      iex> Decimal.compare("1.0", "1.2", "0.1")
      :lt
  """
  @spec compare(decimal :: decimal(), decimal :: decimal(), threshold :: decimal()) ::
          compare_result()

  def compare(_, _, %Decimal{sign: -1}), do: raise(Error, reason: "threshold cannot be negative")

  def compare(%Decimal{} = n1, %Decimal{} = n2, %Decimal{} = threshold) do
    add_threshold = n1 |> Decimal.add(threshold)
    sub_threshold = n1 |> Decimal.sub(threshold)
    case1 = compare(add_threshold, n2)
    case2 = compare(sub_threshold, n2)

    cond do
      (case1 == :gt or case1 == :eq) and (case2 == :lt or case2 == :eq) -> :eq
      case1 == :gt -> :gt
      case2 == :lt -> :lt
    end
  end

  def compare(n1, n2, threshold), do: compare(decimal(n1), decimal(n2), decimal(threshold))

  @doc """
  Compares two numbers numerically. If the first number is greater than the second
  `:gt` is returned, if less than `:lt` is returned, if both numbers are equal
  `:eq` is returned.

  Neither number can be a NaN.

  ## Examples

      iex> Decimal.compare("1.0", 1)
      :eq

      iex> Decimal.compare("Inf", -1)
      :gt

  """
  @spec compare(decimal, decimal) :: compare_result()
  def compare(%Decimal{coef: :inf, sign: sign}, %Decimal{coef: :inf, sign: sign}),
    do: :eq

  def compare(%Decimal{coef: :inf, sign: sign1}, %Decimal{coef: :inf, sign: sign2})
      when sign1 < sign2,
      do: :lt

  def compare(%Decimal{coef: :inf, sign: sign1}, %Decimal{coef: :inf, sign: sign2})
      when sign1 > sign2,
      do: :gt

  def compare(%Decimal{coef: :inf, sign: 1}, _num2), do: :gt
  def compare(%Decimal{coef: :inf, sign: -1}, _num2), do: :lt

  def compare(_num1, %Decimal{coef: :inf, sign: 1}), do: :lt
  def compare(_num1, %Decimal{coef: :inf, sign: -1}), do: :gt

  def compare(%Decimal{coef: :NaN} = num1, _num2),
    do: error(:invalid_operation, "operation on NaN", num1)

  def compare(_num1, %Decimal{coef: :NaN} = num2),
    do: error(:invalid_operation, "operation on NaN", num2)

  def compare(%Decimal{coef: 0}, %Decimal{coef: 0}), do: :eq

  def compare(%Decimal{sign: 1}, %Decimal{coef: 0}), do: :gt
  def compare(%Decimal{coef: 0}, %Decimal{sign: 1}), do: :lt
  def compare(%Decimal{sign: -1}, %Decimal{coef: 0}), do: :lt
  def compare(%Decimal{coef: 0}, %Decimal{sign: -1}), do: :gt

  def compare(%Decimal{sign: 1}, %Decimal{sign: -1}), do: :gt
  def compare(%Decimal{sign: -1}, %Decimal{sign: 1}), do: :lt

  def compare(%Decimal{} = num1, %Decimal{} = num2) do
    adjusted_exp1 = adjust_exp(num1)
    adjusted_exp2 = adjust_exp(num2)

    sign =
      cond do
        adjusted_exp1 == adjusted_exp2 ->
          padded_num1 = pad_num(num1, num1.exp - num2.exp)
          padded_num2 = pad_num(num2, num2.exp - num1.exp)

          cond do
            padded_num1 == padded_num2 -> 0
            padded_num1 < padded_num2 -> -num1.sign
            true -> num1.sign
          end

        adjusted_exp1 < adjusted_exp2 ->
          -num1.sign

        true ->
          num1.sign
      end

    case sign do
      0 -> :eq
      1 -> :gt
      -1 -> :lt
    end
  end

  def compare(num1, num2) do
    compare(decimal(num1), decimal(num2))
  end

  defp adjust_exp(%Decimal{coef: coef, exp: exp}) do
    coef_adjustment = coef_length(coef)
    exp + coef_adjustment - 1
  end

  defp coef_length(0), do: 1
  defp coef_length(coef), do: coef_length(coef, 0)

  defp coef_length(0, length), do: length
  defp coef_length(coef, length), do: coef_length(Kernel.div(coef, 10), length + 1)

  defp pad_num(%Decimal{coef: coef}, n) do
    coef * pow10(Kernel.max(n, 0) + 1)
  end

  @deprecated "Use compare/2 instead"
  @spec cmp(decimal, decimal) :: :lt | :eq | :gt
  def cmp(num1, num2) do
    compare(num1, num2)
  end

  @doc """
  Compares two numbers numerically and returns `true` if they are equal,
  otherwise `false`. If one of the operands is a quiet NaN this operation
  will always return `false`.

  ## Examples

      iex> Decimal.equal?("1.0", 1)
      true

      iex> Decimal.equal?(1, -1)
      false

  """
  @spec equal?(decimal, decimal) :: boolean
  def equal?(num1, num2) do
    eq?(num1, num2)
  end

  @doc """
  Compares two numbers numerically and returns `true` if they are equal,
  otherwise `false`. If one of the operands is a quiet NaN this operation
  will always return `false`.

  ## Examples

      iex> Decimal.eq?("1.0", 1)
      true

      iex> Decimal.eq?(1, -1)
      false

  """
  doc_since("1.8.0")
  @spec eq?(decimal, decimal) :: boolean
  def eq?(%Decimal{coef: :NaN}, _num2), do: false
  def eq?(_num1, %Decimal{coef: :NaN}), do: false
  def eq?(num1, num2), do: compare(num1, num2) == :eq

  @doc """
  It compares the equality of two numbers. If the second number is within
  the range of first - threshold and first + threshold, it returns true;
  otherwise, it returns false.

  ## Examples

      iex> Decimal.eq?("1.0", 1, "0")
      true

      iex> Decimal.eq?("1.2", 1, "0.1")
      false

      iex> Decimal.eq?("1.2", 1, "0.2")
      true

      iex> Decimal.eq?(1, -1, "0.0")
      false

  """
  @spec eq?(decimal :: decimal(), decimal :: decimal(), thresrold :: decimal()) :: boolean()
  def eq?(num1, num2, thresrold), do: compare(num1, num2, thresrold) == :eq

  @doc """
  Compares two numbers numerically and returns `true` if the first argument
  is greater than the second, otherwise `false`. If one the operands is a
  quiet NaN this operation will always return `false`.

  ## Examples

      iex> Decimal.gt?("1.3", "1.2")
      true

      iex> Decimal.gt?("1.2", "1.3")
      false

  """
  doc_since("1.8.0")
  @spec gt?(decimal, decimal) :: boolean
  def gt?(%Decimal{coef: :NaN}, _num2), do: false
  def gt?(_num1, %Decimal{coef: :NaN}), do: false
  def gt?(num1, num2), do: compare(num1, num2) == :gt

  @doc """
  Compares two numbers numerically and returns `true` if the first number is
  less than the second number, otherwise `false`. If one of the operands is a
  quiet NaN this operation will always return `false`.

  ## Examples

      iex> Decimal.lt?("1.1", "1.2")
      true

      iex> Decimal.lt?("1.4", "1.2")
      false

  """
  doc_since("1.8.0")
  @spec lt?(decimal, decimal) :: boolean
  def lt?(%Decimal{coef: :NaN}, _num2), do: false
  def lt?(_num1, %Decimal{coef: :NaN}), do: false
  def lt?(num1, num2), do: compare(num1, num2) == :lt

  @doc """
  Compares two numbers numerically and returns `true` if
  the first argument is greater than or equal the second,
  otherwise `false`.

  If one the operands is a quiet NaN this operation
  will always return `false`.

  ## Examples

      iex> Decimal.gte?("1.3", "1.3")
      true

      iex> Decimal.gte?("1.3", "1.2")
      true

      iex> Decimal.gte?("1.2", "1.3")
      false

  """
  doc_since("2.2.0")
  @spec gte?(decimal, decimal) :: boolean

  def gte?(%Decimal{coef: :NaN}, _num2), do: false
  def gte?(_num1, %Decimal{coef: :NaN}), do: false

  def gte?(num1, num2) do
    case compare(num1, num2) do
      :gt -> true
      :eq -> true
      _ -> false
    end
  end

  @doc """
  Compares two numbers numerically and returns `true` if
  the first number is less than or equal the second number,
  otherwise `false`.

  If one of the operands is a quiet NaN this operation
  will always return `false`.

  ## Examples

      iex> Decimal.lte?("1.1", "1.1")
      true

      iex> Decimal.lte?("1.1", "1.2")
      true

      iex> Decimal.lte?("1.4", "1.2")
      false

  """
  doc_since("2.2.0")
  @spec lte?(decimal, decimal) :: boolean

  def lte?(%Decimal{coef: :NaN}, _num2), do: false
  def lte?(_num1, %Decimal{coef: :NaN}), do: false

  def lte?(num1, num2) do
    case compare(num1, num2) do
      :lt -> true
      :eq -> true
      _ -> false
    end
  end

  @doc """
  Divides two numbers.

  ## Exceptional conditions

    * If both numbers are ±Infinity `:invalid_operation` is signalled.
    * If both numbers are ±0 `:invalid_operation` is signalled.
    * If second number (denominator) is ±0 `:division_by_zero` is signalled.

  ## Examples

      iex> Decimal.div(3, 4)
      Decimal.new("0.75")

      iex> Decimal.div("Inf", -1)
      Decimal.new("-Infinity")

  """
  @spec div(decimal, decimal) :: t
  def div(%Decimal{coef: :NaN} = num1, %Decimal{}), do: num1

  def div(%Decimal{}, %Decimal{coef: :NaN} = num2), do: num2

  def div(%Decimal{coef: :inf}, %Decimal{coef: :inf}),
    do: error(:invalid_operation, "±Infinity / ±Infinity", %Decimal{coef: :NaN})

  def div(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    %{num1 | sign: sign}
  end

  def div(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # TODO: Subnormal
    # exponent?
    %Decimal{sign: sign, coef: 0, exp: exp1 - exp2}
  end

  def div(%Decimal{coef: 0}, %Decimal{coef: 0}),
    do: error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})

  def div(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    sign = if sign1 == sign2, do: 1, else: -1
    error(:division_by_zero, nil, %Decimal{sign: sign, coef: :inf})
  end

  def div(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2
    sign = if sign1 == sign2, do: 1, else: -1

    if coef1 == 0 do
      context(%Decimal{sign: sign, coef: 0, exp: exp1 - exp2}, [])
    else
      prec10 = pow10(Context.get().precision)
      {coef1, coef2, adjust} = div_adjust(coef1, coef2, 0)
      {coef, adjust, _rem, signals} = div_calc(coef1, coef2, 0, adjust, prec10)

      context(%Decimal{sign: sign, coef: coef, exp: exp1 - exp2 - adjust}, signals)
    end
  end

  def div(num1, num2) do
    div(decimal(num1), decimal(num2))
  end

  @doc """
  Divides two numbers and returns the integer part.

  ## Exceptional conditions

    * If both numbers are ±Infinity `:invalid_operation` is signalled.
    * If both numbers are ±0 `:invalid_operation` is signalled.
    * If second number (denominator) is ±0 `:division_by_zero` is signalled.

  ## Examples

      iex> Decimal.div_int(5, 2)
      Decimal.new("2")

      iex> Decimal.div_int("Inf", -1)
      Decimal.new("-Infinity")

  """
  @spec div_int(decimal, decimal) :: t
  def div_int(%Decimal{coef: :NaN} = num1, %Decimal{}), do: num1

  def div_int(%Decimal{}, %Decimal{coef: :NaN} = num2), do: num2

  def div_int(%Decimal{coef: :inf}, %Decimal{coef: :inf}),
    do: error(:invalid_operation, "±Infinity / ±Infinity", %Decimal{coef: :NaN})

  def div_int(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    %{num1 | sign: sign}
  end

  def div_int(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # TODO: Subnormal
    # exponent?
    %Decimal{sign: sign, coef: 0, exp: exp1 - exp2}
  end

  def div_int(%Decimal{coef: 0}, %Decimal{coef: 0}),
    do: error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})

  def div_int(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    div_sign = if sign1 == sign2, do: 1, else: -1
    error(:division_by_zero, nil, %Decimal{sign: div_sign, coef: :inf})
  end

  def div_int(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2
    div_sign = if sign1 == sign2, do: 1, else: -1

    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == :lt ->
        %Decimal{sign: div_sign, coef: 0, exp: exp1 - exp2}

      coef1 == 0 ->
        context(%{num1 | sign: div_sign})

      true ->
        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            result

          {:error, error, reason, num} ->
            error(error, reason, num)
        end
    end
  end

  def div_int(num1, num2) do
    div_int(decimal(num1), decimal(num2))
  end

  @doc """
  Remainder of integer division of two numbers. The result will have the sign of
  the first number.

  ## Exceptional conditions

    * If both numbers are ±Infinity `:invalid_operation` is signalled.
    * If both numbers are ±0 `:invalid_operation` is signalled.
    * If second number (denominator) is ±0 `:division_by_zero` is signalled.

  ## Examples

      iex> Decimal.rem(5, 2)
      Decimal.new("1")

  """
  @spec rem(decimal, decimal) :: t
  def rem(%Decimal{coef: :NaN} = num1, %Decimal{}), do: num1

  def rem(%Decimal{}, %Decimal{coef: :NaN} = num2), do: num2

  def rem(%Decimal{coef: :inf}, %Decimal{coef: :inf}),
    do: error(:invalid_operation, "±Infinity / ±Infinity", %Decimal{coef: :NaN})

  def rem(%Decimal{sign: sign1, coef: :inf}, %Decimal{}), do: %Decimal{sign: sign1, coef: 0}

  def rem(%Decimal{sign: sign1}, %Decimal{coef: :inf} = num2) do
    # TODO: Subnormal
    # exponent?
    %{num2 | sign: sign1}
  end

  def rem(%Decimal{coef: 0}, %Decimal{coef: 0}),
    do: error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})

  def rem(%Decimal{sign: sign1}, %Decimal{coef: 0}),
    do: error(:division_by_zero, nil, %Decimal{sign: sign1, coef: 0})

  def rem(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2

    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == :lt ->
        %{num1 | sign: sign1}

      coef1 == 0 ->
        context(%{num2 | sign: sign1})

      true ->
        div_sign = if sign1 == sign2, do: 1, else: -1

        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            sub(num1, mult(num2, result))

          {:error, error, reason, num} ->
            error(error, reason, num)
        end
    end
  end

  def rem(num1, num2) do
    rem(decimal(num1), decimal(num2))
  end

  @doc """
  Integer division of two numbers and the remainder. Should be used when both
  `div_int/2` and `rem/2` is needed. Equivalent to: `{Decimal.div_int(x, y),
  Decimal.rem(x, y)}`.

  ## Exceptional conditions

    * If both numbers are ±Infinity `:invalid_operation` is signalled.
    * If both numbers are ±0 `:invalid_operation` is signalled.
    * If second number (denominator) is ±0 `:division_by_zero` is signalled.

  ## Examples

      iex> Decimal.div_rem(5, 2)
      {Decimal.new(2), Decimal.new(1)}

  """
  @spec div_rem(decimal, decimal) :: {t, t}
  def div_rem(%Decimal{coef: :NaN} = num1, %Decimal{}), do: {num1, num1}

  def div_rem(%Decimal{}, %Decimal{coef: :NaN} = num2), do: {num2, num2}

  def div_rem(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    numbers = {%Decimal{coef: :NaN}, %Decimal{coef: :NaN}}
    error(:invalid_operation, "±Infinity / ±Infinity", numbers)
  end

  def div_rem(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    {%{num1 | sign: sign}, %Decimal{sign: sign1, coef: 0}}
  end

  def div_rem(%Decimal{} = num1, %Decimal{coef: :inf} = num2) do
    %Decimal{sign: sign1, exp: exp1} = num1
    %Decimal{sign: sign2, exp: exp2} = num2

    sign = if sign1 == sign2, do: 1, else: -1
    # TODO: Subnormal
    # exponent?
    {%Decimal{sign: sign, coef: 0, exp: exp1 - exp2}, %{num2 | sign: sign1}}
  end

  def div_rem(%Decimal{coef: 0}, %Decimal{coef: 0}) do
    error = error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})
    {error, error}
  end

  def div_rem(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    div_sign = if sign1 == sign2, do: 1, else: -1
    div_error = error(:division_by_zero, nil, %Decimal{sign: div_sign, coef: :inf})
    rem_error = error(:division_by_zero, nil, %Decimal{sign: sign1, coef: 0})
    {div_error, rem_error}
  end

  def div_rem(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2
    div_sign = if sign1 == sign2, do: 1, else: -1

    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == :lt ->
        {%Decimal{sign: div_sign, coef: 0, exp: exp1 - exp2}, %{num1 | sign: sign1}}

      coef1 == 0 ->
        {context(%{num1 | sign: div_sign}), context(%{num2 | sign: sign1})}

      true ->
        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            {result, sub(num1, mult(num2, result))}

          {:error, error, reason, num} ->
            error(error, reason, {num, num})
        end
    end
  end

  def div_rem(num1, num2) do
    div_rem(decimal(num1), decimal(num2))
  end

  @doc """
  Compares two values numerically and returns the maximum. Unlike most other
  functions in `Decimal` if a number is NaN the result will be the other number.
  Only if both numbers are NaN will NaN be returned.

  ## Examples

      iex> Decimal.max(1, "2.0")
      Decimal.new("2.0")

      iex> Decimal.max(1, "NaN")
      Decimal.new("1")

      iex> Decimal.max("NaN", "NaN")
      Decimal.new("NaN")

  """
  @spec max(decimal, decimal) :: t
  def max(%Decimal{coef: :NaN}, %Decimal{} = num2), do: num2

  def max(%Decimal{} = num1, %Decimal{coef: :NaN}), do: num1

  def max(%Decimal{sign: sign1, exp: exp1} = num1, %Decimal{sign: sign2, exp: exp2} = num2) do
    case compare(num1, num2) do
      :lt ->
        num2

      :gt ->
        num1

      :eq ->
        cond do
          sign1 != sign2 ->
            if sign1 == 1, do: num1, else: num2

          sign1 == 1 ->
            if exp1 > exp2, do: num1, else: num2

          sign1 == -1 ->
            if exp1 < exp2, do: num1, else: num2
        end
    end
    |> context()
  end

  def max(num1, num2) do
    max(decimal(num1), decimal(num2))
  end

  @doc """
  Compares two values numerically and returns the minimum. Unlike most other
  functions in `Decimal` if a number is NaN the result will be the other number.
  Only if both numbers are NaN will NaN be returned.

  ## Examples

      iex> Decimal.min(1, "2.0")
      Decimal.new("1")

      iex> Decimal.min(1, "NaN")
      Decimal.new("1")

      iex> Decimal.min("NaN", "NaN")
      Decimal.new("NaN")

  """
  @spec min(decimal, decimal) :: t
  def min(%Decimal{coef: :NaN}, %Decimal{} = num2), do: num2

  def min(%Decimal{} = num1, %Decimal{coef: :NaN}), do: num1

  def min(%Decimal{sign: sign1, exp: exp1} = num1, %Decimal{sign: sign2, exp: exp2} = num2) do
    case compare(num1, num2) do
      :lt ->
        num1

      :gt ->
        num2

      :eq ->
        cond do
          sign1 != sign2 ->
            if sign1 == -1, do: num1, else: num2

          sign1 == 1 ->
            if exp1 < exp2, do: num1, else: num2

          sign1 == -1 ->
            if exp1 > exp2, do: num1, else: num2
        end
    end
    |> context()
  end

  def min(num1, num2) do
    min(decimal(num1), decimal(num2))
  end

  @doc """
  Negates the given number.

  ## Examples

      iex> Decimal.negate(1)
      Decimal.new("-1")

      iex> Decimal.negate("-Inf")
      Decimal.new("Infinity")

  """
  doc_since("1.9.0")
  @spec negate(decimal) :: t
  def negate(%Decimal{coef: :NaN} = num), do: num
  def negate(%Decimal{sign: sign} = num), do: context(%{num | sign: -sign})
  def negate(num), do: negate(decimal(num))

  @doc """
  Applies the context to the given number rounding it to specified precision.
  """
  doc_since("1.9.0")
  @spec apply_context(t) :: t
  def apply_context(%Decimal{} = num), do: context(num)

  @doc """
  Returns `true` if given number is positive, otherwise `false`.

  ## Examples

      iex> Decimal.positive?(Decimal.new("42"))
      true

      iex> Decimal.positive?(Decimal.new("-42"))
      false

      iex> Decimal.positive?(Decimal.new("0"))
      false

      iex> Decimal.positive?(Decimal.new("NaN"))
      false

  """
  doc_since("1.5.0")
  @spec positive?(t) :: boolean
  def positive?(%Decimal{coef: :NaN}), do: false
  def positive?(%Decimal{coef: 0}), do: false
  def positive?(%Decimal{sign: -1}), do: false
  def positive?(%Decimal{sign: 1}), do: true

  @doc """
  Returns `true` if given number is negative, otherwise `false`.

  ## Examples

      iex> Decimal.negative?(Decimal.new("-42"))
      true

      iex> Decimal.negative?(Decimal.new("42"))
      false

      iex> Decimal.negative?(Decimal.new("0"))
      false

      iex> Decimal.negative?(Decimal.new("NaN"))
      false

  """
  doc_since("1.5.0")
  @spec negative?(t) :: boolean
  def negative?(%Decimal{coef: :NaN}), do: false
  def negative?(%Decimal{coef: 0}), do: false
  def negative?(%Decimal{sign: 1}), do: false
  def negative?(%Decimal{sign: -1}), do: true

  @doc """
  Multiplies two numbers.

  ## Exceptional conditions

    * If one number is ±0 and the other is ±Infinity `:invalid_operation` is
      signalled.

  ## Examples

      iex> Decimal.mult("0.5", 3)
      Decimal.new("1.5")

      iex> Decimal.mult("Inf", -1)
      Decimal.new("-Infinity")

  """
  @spec mult(decimal, decimal) :: t
  def mult(%Decimal{coef: :NaN} = num1, %Decimal{}), do: num1

  def mult(%Decimal{}, %Decimal{coef: :NaN} = num2), do: num2

  def mult(%Decimal{coef: 0}, %Decimal{coef: :inf}),
    do: error(:invalid_operation, "0 * ±Infinity", %Decimal{coef: :NaN})

  def mult(%Decimal{coef: :inf}, %Decimal{coef: 0}),
    do: error(:invalid_operation, "0 * ±Infinity", %Decimal{coef: :NaN})

  def mult(%Decimal{sign: sign1, coef: :inf, exp: exp1}, %Decimal{sign: sign2, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # exponent?
    %Decimal{sign: sign, coef: :inf, exp: exp1 + exp2}
  end

  def mult(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # exponent?
    %Decimal{sign: sign, coef: :inf, exp: exp1 + exp2}
  end

  def mult(%Decimal{} = num1, %Decimal{} = num2) do
    %Decimal{sign: sign1, coef: coef1, exp: exp1} = num1
    %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2
    sign = if sign1 == sign2, do: 1, else: -1
    %Decimal{sign: sign, coef: coef1 * coef2, exp: exp1 + exp2} |> context()
  end

  def mult(num1, num2) do
    mult(decimal(num1), decimal(num2))
  end

  @doc """
  Normalizes the given decimal: removes trailing zeros from coefficient while
  keeping the number numerically equivalent by increasing the exponent.

  ## Examples

      iex> Decimal.normalize(Decimal.new("1.00"))
      Decimal.new("1")

      iex> Decimal.normalize(Decimal.new("1.01"))
      Decimal.new("1.01")

  """
  doc_since("1.9.0")
  @spec normalize(t) :: t
  def normalize(%Decimal{coef: :NaN} = num), do: num

  def normalize(%Decimal{coef: :inf} = num) do
    # exponent?
    %{num | exp: 0}
  end

  def normalize(%Decimal{sign: sign, coef: coef, exp: exp}) do
    if coef == 0 do
      %Decimal{sign: sign, coef: 0, exp: 0}
    else
      %{do_normalize(coef, exp) | sign: sign} |> context
    end
  end

  @doc """
  Rounds the given number to specified decimal places with the given strategy
  (default is to round to nearest one). If places is negative, at least that
  many digits to the left of the decimal point will be zero.

  See `Decimal.Context` for more information about rounding algorithms.

  ## Examples

      iex> Decimal.round("1.234")
      Decimal.new("1")

      iex> Decimal.round("1.234", 1)
      Decimal.new("1.2")

  """
  @spec round(decimal, integer, rounding) :: t
  def round(num, places \\ 0, mode \\ :half_up)

  def round(%Decimal{coef: :NaN} = num, _, _), do: num

  def round(%Decimal{coef: :inf} = num, _, _), do: num

  def round(%Decimal{} = num, n, mode) do
    %Decimal{sign: sign, coef: coef, exp: exp} = normalize(num)
    digits = :erlang.integer_to_list(coef)
    target_exp = -n
    value = do_round(sign, digits, exp, target_exp, mode)
    context(value, [])
  end

  def round(num, n, mode) do
    round(decimal(num), n, mode)
  end

  @doc """
  Finds the square root.

  ## Examples

      iex> Decimal.sqrt("100")
      Decimal.new("10")

  """
  doc_since("1.7.0")
  @spec sqrt(decimal) :: t
  def sqrt(%Decimal{coef: :NaN} = num),
    do: error(:invalid_operation, "operation on NaN", num)

  def sqrt(%Decimal{coef: 0, exp: exp} = num),
    do: %{num | exp: exp >>> 1}

  def sqrt(%Decimal{sign: -1} = num),
    do: error(:invalid_operation, "less than zero", num)

  def sqrt(%Decimal{sign: 1, coef: :inf} = num),
    do: num

  def sqrt(%Decimal{sign: 1, coef: coef, exp: exp}) do
    precision = Context.get().precision + 1
    digits = :erlang.integer_to_list(coef)
    num_digits = length(digits)

    # Since the root is calculated from integer operations only, it must be
    # large enough to contain the desired precision. Calculate the amount of
    # `shift` required (powers of 10).
    case exp &&& 1 do
      0 ->
        # To get the desired `shift`, subtract the precision of `coef`'s square
        # root from the desired precision.
        #
        # If `coef` is 10_000, the root is 100 (3 digits of precision).
        # If `coef` is 100, the root is 10 (2 digits of precision).
        shift = precision - ((num_digits + 1) >>> 1)
        sqrt(coef, shift, exp)

      _ ->
        # If `exp` is odd, multiply `coef` by 10 and reduce shift by 1/2. `exp`
        # must be even so the root's exponent is an integer.
        shift = precision - ((num_digits >>> 1) + 1)
        sqrt(coef * 10, shift, exp)
    end
  end

  def sqrt(num) do
    sqrt(decimal(num))
  end

  defp sqrt(coef, shift, exp) do
    if shift >= 0 do
      # shift `coef` up by `shift * 2` digits
      sqrt(coef * pow10(shift <<< 1), shift, exp, true)
    else
      # shift `coef` down by `shift * 2` digits
      operand = pow10(-shift <<< 1)
      sqrt(Kernel.div(coef, operand), shift, exp, Kernel.rem(coef, operand) === 0)
    end
  end

  defp sqrt(shifted_coef, shift, exp, exact) do
    # the preferred exponent is `exp / 2` as per IEEE 754
    exp = exp >>> 1
    # guess a root 10x higher than desired precision
    guess = pow10(Context.get().precision + 1)
    root = sqrt_loop(shifted_coef, guess)

    if exact and root * root === shifted_coef do
      # if the root is exact, use preferred `exp` and shift `coef` to match
      coef =
        if shift >= 0,
          do: Kernel.div(root, pow10(shift)),
          else: root * pow10(-shift)

      context(%Decimal{sign: 1, coef: coef, exp: exp})
    else
      # otherwise the calculated root is inexact (but still meets precision),
      # so use the root as `coef` and get the final exponent by shifting `exp`
      context(%Decimal{sign: 1, coef: root, exp: exp - shift})
    end
  end

  # Babylonion method
  defp sqrt_loop(coef, guess) do
    quotient = Kernel.div(coef, guess)

    if guess <= quotient do
      guess
    else
      sqrt_loop(coef, (guess + quotient) >>> 1)
    end
  end

  @doc """
  Creates a new decimal number from an integer or a string representation.

  A decimal number will always be created exactly as specified with all digits
  kept - it will not be rounded with the context.

  ## Backus–Naur form

      sign           ::=  "+" | "-"
      digit          ::=  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
      indicator      ::=  "e" | "E"
      digits         ::=  digit [digit]...
      decimal-part   ::=  digits "." [digits] | ["."] digits
      exponent-part  ::=  indicator [sign] digits
      infinity       ::=  "Infinity" | "Inf"
      nan            ::=  "NaN" [digits]
      numeric-value  ::=  decimal-part [exponent-part] | infinity
      numeric-string ::=  [sign] numeric-value | [sign] nan

  ## Floats

  See also `from_float/1`.

  ## Examples

      iex> Decimal.new(1)
      Decimal.new("1")

      iex> Decimal.new("3.14")
      Decimal.new("3.14")

  """
  @spec new(decimal) :: t
  def new(%Decimal{sign: sign, coef: coef, exp: exp} = num)
      when sign in [1, -1] and ((is_integer(coef) and coef >= 0) or coef in [:NaN, :inf]) and
             is_integer(exp),
      do: num

  def new(int) when is_integer(int),
    do: %Decimal{sign: if(int < 0, do: -1, else: 1), coef: Kernel.abs(int)}

  def new(binary) when is_binary(binary) do
    case parse(binary) do
      {decimal, ""} -> decimal
      _ -> raise Error, reason: "number parsing syntax: #{inspect(binary)}"
    end
  end

  @doc """
  Creates a new decimal number from the sign, coefficient and exponent such that
  the number will be: `sign * coefficient * 10 ^ exponent`.

  A decimal number will always be created exactly as specified with all digits
  kept - it will not be rounded with the context.

  ## Examples

      iex> Decimal.new(1, 42, 0)
      Decimal.new("42")

  """
  @spec new(sign :: 1 | -1, coef :: non_neg_integer | :NaN | :inf, exp :: integer) :: t
  def new(sign, coef, exp)
      when sign in [1, -1] and ((is_integer(coef) and coef >= 0) or coef in [:NaN, :inf]) and
             is_integer(exp),
      do: %Decimal{sign: sign, coef: coef, exp: exp}

  @doc """
  Creates a new decimal number from a floating point number.

  Floating point numbers use a fixed number of binary digits to represent
  a decimal number which has inherent inaccuracy as some decimal numbers cannot
  be represented exactly in limited precision binary.

  Floating point numbers will be converted to decimal numbers with
  `:io_lib_format.fwrite_g/1`. Since this conversion is not exact and
  because of inherent inaccuracy mentioned above, we may run into counter-intuitive results:

      iex> Enum.reduce([0.1, 0.1, 0.1], &+/2)
      0.30000000000000004

      iex> Enum.reduce([Decimal.new("0.1"), Decimal.new("0.1"), Decimal.new("0.1")], &Decimal.add/2)
      Decimal.new("0.3")

  For this reason, it's recommended to build decimals with `new/1`, which is always precise, instead.

  ## Examples

      iex> Decimal.from_float(3.14)
      Decimal.new("3.14")

  """
  doc_since("1.5.0")
  @spec from_float(float) :: t
  def from_float(float) when is_float(float) do
    float
    |> :io_lib_format.fwrite_g()
    |> fix_float_exp()
    |> IO.iodata_to_binary()
    |> new()
  end

  @doc """
  Creates a new decimal number from an integer, string, float, or existing decimal number.

  Because conversion from a floating point number is not exact, it's recommended
  to instead use `new/1` or `from_float/1` when the argument's type is certain.
  See `from_float/1`.

  ## Examples

      iex> {:ok, decimal} = Decimal.cast(3)
      iex> decimal
      Decimal.new("3")

      iex> Decimal.cast("bad")
      :error

  """
  @spec cast(term) :: {:ok, t} | :error
  def cast(integer) when is_integer(integer), do: {:ok, Decimal.new(integer)}
  def cast(%Decimal{} = decimal), do: {:ok, decimal}
  def cast(float) when is_float(float), do: {:ok, from_float(float)}

  def cast(binary) when is_binary(binary) do
    case parse(binary) do
      {decimal, ""} -> {:ok, decimal}
      _ -> :error
    end
  end

  def cast(_), do: :error

  @doc """
  Parses a binary into a decimal.

  If successful, returns a tuple in the form of `{decimal, remainder_of_binary}`,
  otherwise `:error`.

  ## Examples

      iex> Decimal.parse("3.14")
      {%Decimal{coef: 314, exp: -2, sign: 1}, ""}

      iex> Decimal.parse("3.14.15")
      {%Decimal{coef: 314, exp: -2, sign: 1}, ".15"}

      iex> Decimal.parse("-1.1e3")
      {%Decimal{coef: 11, exp: 2, sign: -1}, ""}

      iex> Decimal.parse("bad")
      :error

  """
  @spec parse(binary()) :: {t(), binary()} | :error
  def parse("+" <> rest) do
    parse_unsign(rest)
  end

  def parse("-" <> rest) do
    case parse_unsign(rest) do
      {%Decimal{} = num, rest} -> {%{num | sign: -1}, rest}
      :error -> :error
    end
  end

  def parse(binary) when is_binary(binary) do
    parse_unsign(binary)
  end

  @doc """
  Converts given number to its string representation.

  ## Options

    * `:scientific` - number converted to scientific notation.
    * `:normal` - number converted without a exponent.
    * `:xsd` - number converted to the [canonical XSD representation](https://www.w3.org/TR/xmlschema-2/#decimal).
    * `:raw` - number converted to its raw, internal format.

  ## Examples

      iex> Decimal.to_string(Decimal.new("1.00"))
      "1.00"

      iex> Decimal.to_string(Decimal.new("123e1"), :scientific)
      "1.23E+3"

      iex> Decimal.to_string(Decimal.new("42.42"), :normal)
      "42.42"

      iex> Decimal.to_string(Decimal.new("1.00"), :xsd)
      "1.0"

      iex> Decimal.to_string(Decimal.new("4321.768"), :raw)
      "4321768E-3"

  """
  @spec to_string(t, :scientific | :normal | :xsd | :raw) :: String.t()
  def to_string(num, type \\ :scientific)

  def to_string(%Decimal{sign: sign, coef: :NaN}, _type) do
    if sign == 1, do: "NaN", else: "-NaN"
  end

  def to_string(%Decimal{sign: sign, coef: :inf}, _type) do
    if sign == 1, do: "Infinity", else: "-Infinity"
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :normal) do
    list = integer_to_charlist(coef)

    list =
      if exp >= 0 do
        list ++ :lists.duplicate(exp, ?0)
      else
        diff = length(list) + exp

        if diff > 0 do
          List.insert_at(list, diff, ?.)
        else
          ~c"0." ++ :lists.duplicate(-diff, ?0) ++ list
        end
      end

    list = if sign == -1, do: [?- | list], else: list
    IO.iodata_to_binary(list)
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :scientific) do
    list = integer_to_charlist(coef)
    length = length(list)
    adjusted = exp + length - 1

    list =
      cond do
        exp == 0 ->
          list

        exp < 0 and adjusted >= -6 ->
          abs_exp = Kernel.abs(exp)
          diff = -length + abs_exp + 1

          if diff > 0 do
            list = :lists.duplicate(diff, ?0) ++ list
            List.insert_at(list, 1, ?.)
          else
            List.insert_at(list, exp - 1, ?.)
          end

        true ->
          list = if length > 1, do: List.insert_at(list, 1, ?.), else: list
          list = list ++ ~c"E"
          list = if exp >= 0, do: list ++ ~c"+", else: list
          list ++ integer_to_charlist(adjusted)
      end

    list = if sign == -1, do: [?- | list], else: list
    IO.iodata_to_binary(list)
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :raw) do
    str = Integer.to_string(coef)
    str = if sign == -1, do: [?- | str], else: str
    str = if exp != 0, do: [str, "E", Integer.to_string(exp)], else: str

    IO.iodata_to_binary(str)
  end

  def to_string(%Decimal{} = decimal, :xsd) do
    decimal |> canonical_xsd() |> to_string(:normal)
  end

  defp canonical_xsd(%Decimal{coef: 0} = decimal), do: %{decimal | exp: -1}

  defp canonical_xsd(%Decimal{coef: coef, exp: 0} = decimal),
    do: %{decimal | coef: coef * 10, exp: -1}

  defp canonical_xsd(%Decimal{coef: coef, exp: exp} = decimal)
       when exp > 0,
       do: canonical_xsd(%{decimal | coef: coef * 10, exp: exp - 1})

  defp canonical_xsd(%Decimal{coef: coef} = decimal)
       when Kernel.rem(coef, 10) != 0,
       do: decimal

  defp canonical_xsd(%Decimal{coef: coef, exp: exp} = decimal),
    do: canonical_xsd(%{decimal | coef: Kernel.div(coef, 10), exp: exp + 1})

  @doc """
  Returns the decimal represented as an integer.

  Fails when loss of precision will occur.

  ## Examples

      iex> Decimal.to_integer(Decimal.new("42"))
      42

      iex> Decimal.to_integer(Decimal.new("1.00"))
      1

      iex> Decimal.to_integer(Decimal.new("1.10"))
      ** (ArgumentError) cannot convert Decimal.new("1.1") without losing precision. Use Decimal.round/3 first.

  """
  @spec to_integer(t) :: integer
  def to_integer(%Decimal{sign: sign, coef: coef, exp: 0})
      when is_integer(coef),
      do: sign * coef

  def to_integer(%Decimal{sign: sign, coef: coef, exp: exp})
      when is_integer(coef) and exp > 0,
      do: to_integer(%Decimal{sign: sign, coef: coef * 10, exp: exp - 1})

  def to_integer(%Decimal{sign: sign, coef: coef, exp: exp})
      when is_integer(coef) and exp < 0 and Kernel.rem(coef, 10) == 0,
      do: to_integer(%Decimal{sign: sign, coef: Kernel.div(coef, 10), exp: exp + 1})

  def to_integer(%Decimal{coef: coef} = decimal) when is_integer(coef) do
    raise ArgumentError,
          "cannot convert #{inspect(decimal)} without losing precision. Use Decimal.round/3 first."
  end

  @doc """
  Returns the decimal converted to a float.

  The returned float may have lower precision than the decimal. Fails if
  the decimal cannot be converted to a float.

  ## Examples

      iex> Decimal.to_float(Decimal.new("1.5"))
      1.5

  """
  @spec to_float(t) :: float
  def to_float(%Decimal{sign: sign, coef: coef, exp: exp}) when is_integer(coef) do
    # Convert back to float without loss
    # http://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
    {num, den} = ratio(coef, exp)

    boundary = den <<< 52

    cond do
      num == 0 ->
        0.0

      num >= boundary ->
        {den, exp} = scale_down(num, boundary, 52)
        decimal_to_float(sign, num, den, exp)

      true ->
        {num, exp} = scale_up(num, boundary, 52)
        decimal_to_float(sign, num, den, exp)
    end
  end

  @doc """
  Returns the scale of the decimal.

  A decimal's scale is the number of digits after the decimal point. This
  includes trailing zeros; see `normalize/1` to remove them.

  ## Examples

      iex> Decimal.scale(Decimal.new("42"))
      0

      iex> Decimal.scale(Decimal.new(1, 2, 26))
      0

      iex> Decimal.scale(Decimal.new("99.12345"))
      5

      iex> Decimal.scale(Decimal.new("1.50"))
      2
  """
  @spec scale(t) :: non_neg_integer()
  def scale(%Decimal{exp: exp}), do: Kernel.max(0, -exp)

  defp scale_up(num, den, exp) when num >= den, do: {num, exp}
  defp scale_up(num, den, exp), do: scale_up(num <<< 1, den, exp - 1)

  defp scale_down(num, den, exp) do
    new_den = den <<< 1

    if num < new_den do
      {den >>> 52, exp}
    else
      scale_down(num, new_den, exp + 1)
    end
  end

  defp decimal_to_float(sign, num, den, exp) do
    quo = Kernel.div(num, den)
    rem = num - quo * den

    tmp =
      case den >>> 1 do
        den when rem > den -> quo + 1
        den when rem < den -> quo
        _ when (quo &&& 1) === 1 -> quo + 1
        _ -> quo
      end

    sign = if sign == -1, do: 1, else: 0
    tmp = tmp - @power_of_2_to_52
    exp = if tmp < @power_of_2_to_52, do: exp, else: exp + 1
    <<tmp::float>> = <<sign::size(1), exp + 1023::size(11), tmp::size(52)>>
    tmp
  end

  @doc """
  Returns `true` when the given `decimal` has no significant digits after the decimal point.

  ## Examples

      iex> Decimal.integer?("1.00")
      true

      iex> Decimal.integer?("1.10")
      false

  """
  doc_since("2.0.0")
  @spec integer?(decimal()) :: boolean
  def integer?(%Decimal{coef: :NaN}), do: false
  def integer?(%Decimal{coef: :inf}), do: false
  def integer?(%Decimal{coef: coef, exp: exp}), do: exp >= 0 or zero_after_dot?(coef, exp)
  def integer?(num), do: integer?(decimal(num))

  defp zero_after_dot?(coef, exp) when coef >= 10 and exp < 0,
    do: Kernel.rem(coef, 10) == 0 and zero_after_dot?(Kernel.div(coef, 10), exp + 1)

  defp zero_after_dot?(coef, exp),
    do: coef == 0 or exp == 0

  ## ARITHMETIC ##

  defp add_align(coef1, exp1, coef2, exp2) when exp1 == exp2, do: {coef1, coef2}

  defp add_align(coef1, exp1, coef2, exp2) when exp1 > exp2,
    do: {coef1 * pow10(exp1 - exp2), coef2}

  defp add_align(coef1, exp1, coef2, exp2) when exp1 < exp2,
    do: {coef1, coef2 * pow10(exp2 - exp1)}

  defp add_sign(sign1, sign2, coef) do
    cond do
      coef > 0 -> 1
      coef < 0 -> -1
      sign1 == -1 and sign2 == -1 -> -1
      sign1 != sign2 and Context.get().rounding == :floor -> -1
      true -> 1
    end
  end

  defp div_adjust(coef1, coef2, adjust) when coef1 < coef2,
    do: div_adjust(coef1 * 10, coef2, adjust + 1)

  defp div_adjust(coef1, coef2, adjust) when coef1 >= coef2 * 10,
    do: div_adjust(coef1, coef2 * 10, adjust - 1)

  defp div_adjust(coef1, coef2, adjust), do: {coef1, coef2, adjust}

  defp div_calc(coef1, coef2, coef, adjust, prec10) do
    cond do
      coef1 >= coef2 ->
        div_calc(coef1 - coef2, coef2, coef + 1, adjust, prec10)

      coef1 == 0 and adjust >= 0 ->
        {coef, adjust, coef1, []}

      coef >= prec10 ->
        signals = [:rounded]
        signals = if base10?(coef1), do: signals, else: [:inexact | signals]
        {coef, adjust, coef1, signals}

      true ->
        div_calc(coef1 * 10, coef2, coef * 10, adjust + 1, prec10)
    end
  end

  defp div_int_calc(coef1, coef2, coef, adjust, precision) do
    cond do
      coef1 >= coef2 ->
        div_int_calc(coef1 - coef2, coef2, coef + 1, adjust, precision)

      adjust != precision ->
        div_int_calc(coef1 * 10, coef2, coef * 10, adjust + 1, precision)

      true ->
        {coef, coef1}
    end
  end

  defp integer_division(div_sign, coef1, exp1, coef2, exp2) do
    precision = exp1 - exp2
    {coef1, coef2, adjust} = div_adjust(coef1, coef2, 0)

    {coef, _rem} = div_int_calc(coef1, coef2, 0, adjust, precision)

    prec10 = pow10(Context.get().precision)

    if coef > prec10 do
      {
        :error,
        :invalid_operation,
        "integer division impossible, quotient too large",
        %Decimal{coef: :NaN}
      }
    else
      {:ok, %Decimal{sign: div_sign, coef: coef, exp: 0}}
    end
  end

  defp do_normalize(coef, exp) do
    if Kernel.rem(coef, 10) == 0 do
      do_normalize(Kernel.div(coef, 10), exp + 1)
    else
      %Decimal{coef: coef, exp: exp}
    end
  end

  defp ratio(coef, exp) when exp >= 0, do: {coef * pow10(exp), 1}
  defp ratio(coef, exp) when exp < 0, do: {coef, pow10(-exp)}

  pow10_max =
    Enum.reduce(0..104, 1, fn int, acc ->
      defp pow10(unquote(int)), do: unquote(acc)
      defp base10?(unquote(acc)), do: true
      acc * 10
    end)

  defp pow10(num) when num > 104, do: pow10(104) * pow10(num - 104)

  defp base10?(num) when num >= unquote(pow10_max) do
    if Kernel.rem(num, unquote(pow10_max)) == 0 do
      base10?(Kernel.div(num, unquote(pow10_max)))
    else
      false
    end
  end

  defp base10?(_num), do: false

  ## ROUNDING ##

  defp do_round(sign, digits, exp, target_exp, rounding) do
    num_digits = length(digits)
    precision = num_digits - (target_exp - exp)

    cond do
      exp == target_exp ->
        %Decimal{sign: sign, coef: digits_to_integer(digits), exp: exp}

      exp < target_exp and precision < 0 ->
        zeros = :lists.duplicate(target_exp - exp, ?0)
        digits = zeros ++ digits
        {signif, remain} = :lists.split(1, digits)

        signif =
          if increment?(rounding, sign, signif, remain),
            do: digits_increment(signif),
            else: signif

        coef = digits_to_integer(signif)
        %Decimal{sign: sign, coef: coef, exp: target_exp}

      exp < target_exp and precision >= 0 ->
        {signif, remain} = :lists.split(precision, digits)

        signif =
          if increment?(rounding, sign, signif, remain),
            do: digits_increment(signif),
            else: signif

        coef = digits_to_integer(signif)
        %Decimal{sign: sign, coef: coef, exp: target_exp}

      exp > target_exp ->
        digits = digits ++ Enum.map(1..(exp - target_exp), fn _ -> ?0 end)
        coef = digits_to_integer(digits)
        %Decimal{sign: sign, coef: coef, exp: target_exp}
    end
  end

  defp digits_to_integer([]), do: 0
  defp digits_to_integer(digits), do: :erlang.list_to_integer(digits)

  defp precision(%Decimal{coef: :NaN} = num, _precision, _rounding) do
    {num, []}
  end

  defp precision(%Decimal{coef: :inf} = num, _precision, _rounding) do
    {num, []}
  end

  defp precision(%Decimal{sign: sign, coef: coef, exp: exp} = num, precision, rounding) do
    digits = :erlang.integer_to_list(coef)
    num_digits = length(digits)

    if num_digits > precision do
      do_precision(sign, digits, num_digits, exp, precision, rounding)
    else
      {num, []}
    end
  end

  defp do_precision(sign, digits, num_digits, exp, precision, rounding) do
    precision = Kernel.min(num_digits, precision)
    {signif, remain} = :lists.split(precision, digits)

    signif =
      if increment?(rounding, sign, signif, remain), do: digits_increment(signif), else: signif

    signals = if any_nonzero(remain), do: [:inexact, :rounded], else: [:rounded]

    exp = exp + length(remain)
    coef = digits_to_integer(signif)
    dec = %Decimal{sign: sign, coef: coef, exp: exp}
    {dec, signals}
  end

  defp increment?(_, _, _, []), do: false

  defp increment?(:down, _, _, _), do: false

  defp increment?(:up, _, _, _), do: true

  defp increment?(:ceiling, sign, _, remain), do: sign == 1 and any_nonzero(remain)

  defp increment?(:floor, sign, _, remain), do: sign == -1 and any_nonzero(remain)

  defp increment?(:half_up, _, _, [digit | _]), do: digit >= ?5

  defp increment?(:half_even, _, [], [?5 | rest]), do: any_nonzero(rest)

  defp increment?(:half_even, _, signif, [?5 | rest]),
    do: any_nonzero(rest) or Kernel.rem(:lists.last(signif), 2) == 1

  defp increment?(:half_even, _, _, [digit | _]), do: digit > ?5

  defp increment?(:half_down, _, _, [digit | rest]),
    do: digit > ?5 or (digit == ?5 and any_nonzero(rest))

  defp any_nonzero(digits), do: :lists.any(fn digit -> digit != ?0 end, digits)

  defp digits_increment(digits), do: digits_increment(:lists.reverse(digits), [])

  defp digits_increment([?9 | rest], acc), do: digits_increment(rest, [?0 | acc])

  defp digits_increment([head | rest], acc), do: :lists.reverse(rest, [head + 1 | acc])

  defp digits_increment([], acc), do: [?1 | acc]

  ## CONTEXT ##

  defp context(num, signals \\ []) do
    context = Context.get()
    {result, prec_signals} = precision(num, context.precision, context.rounding)
    error(put_uniq(signals, prec_signals), nil, result, context)
  end

  defp put_uniq(list, elems) when is_list(elems) do
    Enum.reduce(elems, list, &put_uniq(&2, &1))
  end

  defp put_uniq(list, elem) do
    if elem in list, do: list, else: [elem | list]
  end

  ## PARSING ##

  defp parse_unsign(<<first, remainder::size(7)-binary, rest::binary>>) when first in [?i, ?I] do
    if String.downcase(remainder) == "nfinity" do
      {%Decimal{coef: :inf}, rest}
    else
      :error
    end
  end

  defp parse_unsign(<<first, remainder::size(2)-binary, rest::binary>>) when first in [?i, ?I] do
    if String.downcase(remainder) == "nf" do
      {%Decimal{coef: :inf}, rest}
    else
      :error
    end
  end

  defp parse_unsign(<<first, remainder::size(2)-binary, rest::binary>>) when first in [?n, ?N] do
    if String.downcase(remainder) == "an" do
      {%Decimal{coef: :NaN}, rest}
    else
      :error
    end
  end

  defp parse_unsign(bin) do
    {int, rest} = parse_digits(bin)
    {float, rest} = parse_float(rest)
    {exp, rest} = parse_exp(rest)

    if int == [] and float == [] do
      :error
    else
      int = if int == [], do: ~c"0", else: int
      exp = if exp == [], do: ~c"0", else: exp

      number = %Decimal{
        coef: List.to_integer(int ++ float),
        exp: List.to_integer(exp) - length(float)
      }

      {number, rest}
    end
  end

  defp parse_float("." <> rest), do: parse_digits(rest)
  defp parse_float(bin), do: {[], bin}

  defp parse_exp(<<e, rest::binary>>) when e in [?e, ?E] do
    case rest do
      <<sign, rest::binary>> when sign in [?+, ?-] ->
        {digits, rest} = parse_digits(rest)
        {[sign | digits], rest}

      _ ->
        parse_digits(rest)
    end
  end

  defp parse_exp(bin) do
    {[], bin}
  end

  defp parse_digits(bin), do: parse_digits(bin, [])

  defp parse_digits(<<digit, rest::binary>>, acc) when digit in ?0..?9 do
    parse_digits(rest, [digit | acc])
  end

  defp parse_digits(rest, acc) do
    {:lists.reverse(acc), rest}
  end

  # Util

  defp decimal(%Decimal{} = num), do: num
  defp decimal(num) when is_integer(num), do: new(num)
  defp decimal(num) when is_binary(num), do: new(num)

  defp decimal(other) when is_float(other) do
    raise ArgumentError,
          "implicit conversion of #{inspect(other)} to Decimal is not allowed. Use Decimal.from_float/1"
  end

  defp handle_error(signals, reason, result, context) do
    context = context || Context.get()
    signals = List.wrap(signals)

    flags = Enum.reduce(signals, context.flags, &put_uniq(&2, &1))
    Context.set(%{context | flags: flags})
    error_signal = Enum.find(signals, &(&1 in context.traps))

    if error_signal do
      error = [signal: error_signal, reason: reason]
      {:error, error}
    else
      {:ok, result}
    end
  end

  defp fix_float_exp(digits) do
    fix_float_exp(digits, [])
  end

  defp fix_float_exp([?e | rest], [?0 | [?. | result]]) do
    fix_float_exp(rest, [?e | result])
  end

  defp fix_float_exp([digit | rest], result) do
    fix_float_exp(rest, [digit | result])
  end

  defp fix_float_exp([], result), do: :lists.reverse(result)

  if Version.compare(System.version(), "1.3.0") == :lt do
    defp integer_to_charlist(string), do: Integer.to_char_list(string)
  else
    defp integer_to_charlist(string), do: Integer.to_charlist(string)
  end
end

defimpl Inspect, for: Decimal do
  def inspect(dec, _opts) do
    "Decimal.new(\"" <> Decimal.to_string(dec) <> "\")"
  end
end

defimpl String.Chars, for: Decimal do
  def to_string(dec) do
    Decimal.to_string(dec)
  end
end

# TODO: remove when we require Elixir 1.18
if Code.ensure_loaded?(JSON.Encoder) and function_exported?(JSON.Encoder, :encode, 2) do
  defimpl JSON.Encoder, for: Decimal do
    def encode(decimal, _encoder) do
      [?", Decimal.to_string(decimal), ?"]
    end
  end
end
