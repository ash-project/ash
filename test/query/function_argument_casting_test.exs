# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.FunctionArgumentCastingTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Query.Function
  alias Ash.Query.Signatures

  @usec_instant ~U[2026-06-15 20:00:00.123456Z]
  @usec_time ~T[20:00:00.123456]

  defmodule Stamp do
    @moduledoc false
    use Ash.Query.Function, name: :stamp

    def args, do: [[:utc_datetime], [:utc_datetime_usec]]
    def returns, do: [:utc_datetime, :utc_datetime_usec]
  end

  defmodule ComputedStamp do
    @moduledoc false
    use Ash.Query.Function, name: :computed_stamp

    def args, do: Enum.map([:utc_datetime_usec], &[&1])
    def returns, do: [:utc_datetime_usec]
  end

  defmodule AtInstant do
    @moduledoc false
    use Ash.CustomExpression, name: :at_instant, arguments: [[:utc_datetime_usec]]

    def expression(_data_layer, _arguments), do: :unknown
  end

  # Casts to a signature as a function casts to its own: with its types initialized.
  defp cast(signature, values, opts \\ []) do
    [signature] = Signatures.initialize([signature])
    Function.try_cast_arguments(signature, values, opts)
  end

  describe "the signatures arguments are cast to" do
    test "are initialized when the module is compiled, if args/0 is a literal" do
      assert [[{:utc_datetime, second}], [{:utc_datetime_usec, microsecond}]] =
               Stamp.__initialized_args__()

      assert second[:precision] == :second
      assert microsecond[:precision] == :microsecond
      assert Function.initialized_args(Stamp) == Stamp.__initialized_args__()
    end

    test "are initialized when they are used, if args/0 is not a literal" do
      assert ComputedStamp.__initialized_args__() == :runtime

      assert [[{:utc_datetime_usec, constraints}]] = Function.initialized_args(ComputedStamp)
      assert constraints[:precision] == :microsecond

      assert {:ok, %ComputedStamp{arguments: [@usec_instant]}} =
               Function.new(ComputedStamp, [@usec_instant])
    end

    test "are initialized when a custom expression is compiled" do
      assert [[{:utc_datetime_usec, constraints}]] =
               Ash.CustomExpression.initialized_arguments(AtInstant)

      assert constraints[:precision] == :microsecond
    end

    test "leave :any, :same and :var_args as they are" do
      assert Signatures.initialize([[:any, :same, {:array, :any}, {:range, :same}]]) ==
               [[:any, :same, {:array, :any}, {:range, :same}]]

      assert Signatures.initialize(:var_args) == :var_args
    end
  end

  describe "a NewType declared in a signature" do
    test ":utc_datetime_usec keeps the microseconds of a value" do
      assert [@usec_instant] = cast([:utc_datetime_usec], [@usec_instant])
    end

    test ":time_usec keeps the microseconds of a value" do
      assert [@usec_time] = cast([:time_usec], [@usec_time])
    end

    test "an array of :utc_datetime_usec keeps the microseconds of each value" do
      assert [[@usec_instant]] =
               cast([{:array, :utc_datetime_usec}], [[@usec_instant]])
    end

    test ":utc_datetime_usec gives a value in seconds microsecond precision" do
      assert [~U[2026-06-15 20:00:00.000000Z]] =
               cast([:utc_datetime_usec], [~U[2026-06-15 20:00:00Z]])
    end

    test "a value in seconds is not an exact match for :utc_datetime_usec" do
      refute cast([:utc_datetime_usec], [~U[2026-06-15 20:00:00Z]], exact?: true)
    end

    test "a value in microseconds is an exact match for :utc_datetime_usec" do
      assert [@usec_instant] =
               cast([:utc_datetime_usec], [@usec_instant], exact?: true)
    end
  end

  describe "a type declared with constraints in a signature" do
    test "the constraints apply whatever order they are written in" do
      declared = {:datetime, cast_dates_as: :start_of_day, precision: :microsecond}

      assert [@usec_instant] = cast([declared], [@usec_instant])
    end

    test "constraints the type rejects leave the value cast as it was before" do
      assert [~U[2026-06-15 20:00:00Z]] =
               cast([{:datetime, precision: :bogus}], [@usec_instant])
    end
  end

  describe "a type declared without constraints in a signature" do
    test ":datetime casts to its default precision, the second" do
      assert [~U[2026-06-15 20:00:00Z]] =
               cast([:datetime], [@usec_instant])
    end

    test ":number is cast without being treated as a type" do
      assert [1.5] = cast([:number], [1.5])
    end
  end

  describe "choosing a signature for a value" do
    test "a value in microseconds selects the :utc_datetime_usec signature" do
      assert {:ok, %Stamp{arguments: [@usec_instant]}} = Function.new(Stamp, [@usec_instant])
    end

    test "a value in seconds selects the :utc_datetime signature" do
      assert {:ok, %Stamp{arguments: [~U[2026-06-15 20:00:00Z]]}} =
               Function.new(Stamp, [~U[2026-06-15 20:00:00Z]])
    end
  end
end
