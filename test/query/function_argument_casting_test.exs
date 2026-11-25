# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.FunctionArgumentCastingTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Query.Function

  @usec_instant ~U[2026-06-15 20:00:00.123456Z]
  @usec_time ~T[20:00:00.123456]

  defmodule Stamp do
    @moduledoc false
    use Ash.Query.Function, name: :stamp

    def args, do: [[:utc_datetime], [:utc_datetime_usec]]
    def returns, do: [:utc_datetime, :utc_datetime_usec]
  end

  describe "a NewType declared in a signature" do
    test ":utc_datetime_usec keeps the microseconds of a value" do
      assert [@usec_instant] = Function.try_cast_arguments([:utc_datetime_usec], [@usec_instant])
    end

    test ":time_usec keeps the microseconds of a value" do
      assert [@usec_time] = Function.try_cast_arguments([:time_usec], [@usec_time])
    end

    test "an array of :utc_datetime_usec keeps the microseconds of each value" do
      assert [[@usec_instant]] =
               Function.try_cast_arguments([{:array, :utc_datetime_usec}], [[@usec_instant]])
    end

    test ":utc_datetime_usec gives a value in seconds microsecond precision" do
      assert [~U[2026-06-15 20:00:00.000000Z]] =
               Function.try_cast_arguments([:utc_datetime_usec], [~U[2026-06-15 20:00:00Z]])
    end

    test "a value in seconds is not an exact match for :utc_datetime_usec" do
      refute Function.try_cast_arguments([:utc_datetime_usec], [~U[2026-06-15 20:00:00Z]],
               exact?: true
             )
    end

    test "a value in microseconds is an exact match for :utc_datetime_usec" do
      assert [@usec_instant] =
               Function.try_cast_arguments([:utc_datetime_usec], [@usec_instant], exact?: true)
    end
  end

  describe "a type declared with constraints in a signature" do
    test "the constraints apply whatever order they are written in" do
      declared = {:datetime, cast_dates_as: :start_of_day, precision: :microsecond}

      assert [@usec_instant] = Function.try_cast_arguments([declared], [@usec_instant])
    end

    test "constraints the type rejects leave the value cast as it was before" do
      assert [~U[2026-06-15 20:00:00Z]] =
               Function.try_cast_arguments([{:datetime, precision: :bogus}], [@usec_instant])
    end
  end

  describe "a type declared without constraints in a signature" do
    test ":datetime casts to its default precision, the second" do
      assert [~U[2026-06-15 20:00:00Z]] =
               Function.try_cast_arguments([:datetime], [@usec_instant])
    end

    test ":number is cast without being treated as a type" do
      assert [1.5] = Function.try_cast_arguments([:number], [1.5])
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
