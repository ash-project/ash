# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.DecimalTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Expr

  defmodule Balance do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :amount, :decimal, allow_nil?: false, public?: true

      attribute :positive_amount, :decimal,
        allow_nil?: false,
        constraints: [greater_than: 0],
        public?: true

      attribute :negative_amount, :decimal,
        allow_nil?: false,
        constraints: [less_than: 0],
        public?: true

      attribute :precise_amount, :decimal,
        allow_nil?: false,
        constraints: [precision: 3],
        public?: true

      attribute :arbitrary_amount, :decimal,
        allow_nil?: false,
        constraints: [precision: :arbitrary],
        public?: true

      attribute :scaled_amount, :decimal,
        allow_nil?: false,
        constraints: [scale: 2],
        public?: true

      attribute :arbitrary_scale_amount, :decimal,
        allow_nil?: false,
        constraints: [scale: :arbitrary],
        public?: true
    end
  end

  @valid_attrs %{
    amount: 1.0,
    positive_amount: 1.0,
    negative_amount: -1.0,
    precise_amount: 1.23,
    arbitrary_amount: 1.234567890,
    scaled_amount: 12.34,
    arbitrary_scale_amount: 1.234567890
  }

  test "pass with valid attrs" do
    assert Balance
           |> Ash.Changeset.for_create(:create, @valid_attrs)
           |> Ash.create!()
  end

  test "fail with invalid positive_amount" do
    for positive_amount <- [0, -1] do
      invalid_attrs = @valid_attrs |> Map.put(:positive_amount, positive_amount)

      assert {:error, %Ash.Error.Invalid{}} =
               Balance
               |> Ash.Changeset.for_create(:create, invalid_attrs)
               |> Ash.create()
    end
  end

  test "fail with invalid negative_amount" do
    for negative_amount <- [0, 1] do
      invalid_attrs = @valid_attrs |> Map.put(:negative_amount, negative_amount)

      assert {:error, %Ash.Error.Invalid{}} =
               Balance
               |> Ash.Changeset.for_create(:create, invalid_attrs)
               |> Ash.create()
    end
  end

  test "treat two decimals same when value is same" do
    assert Ash.Type.Decimal.equal?(Decimal.new("1.0"), Decimal.new("1.00"))
  end

  test "pass with valid precision constraint" do
    valid_attrs = @valid_attrs |> Map.put(:precise_amount, 1.23)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "fail with invalid precision constraint" do
    # 4 significant digits (1234) should fail for precision: 3
    invalid_attrs = @valid_attrs |> Map.put(:precise_amount, 1234)

    assert {:error, %Ash.Error.Invalid{}} =
             Balance
             |> Ash.Changeset.for_create(:create, invalid_attrs)
             |> Ash.create()
  end

  test "allow arbitrary precision when set to :arbitrary" do
    valid_attrs =
      @valid_attrs |> Map.put(:arbitrary_amount, Decimal.new("12345678901234567890.123456789"))

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "precision validation with different decimal formats" do
    # Test that 123 (3 digits) passes precision: 3
    valid_attrs = @valid_attrs |> Map.put(:precise_amount, 123)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()

    # Test that 12.3 (3 significant digits) passes precision: 3
    valid_attrs = @valid_attrs |> Map.put(:precise_amount, 12.3)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()

    # Test that 0.123 (3 significant digits) passes precision: 3
    valid_attrs = @valid_attrs |> Map.put(:precise_amount, 0.123)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "precision validation with string input" do
    # Test direct type casting with precision constraint
    constraints = [precision: 3]

    # Valid cases
    assert {:ok, _} = cast_input("123", constraints)
    assert {:ok, _} = cast_input("12.3", constraints)
    assert {:ok, _} = cast_input("1.23", constraints)

    # Invalid cases (more than 3 significant digits)
    assert {:error, _} = cast_input("1234", constraints)
    assert {:error, _} = cast_input("12.34", constraints)
  end

  test "precision constraint defaults to :arbitrary" do
    # Test that default precision allows any number of digits
    constraints = []

    assert {:ok, _} = cast_input("123456789012345678901234567890", constraints)
  end

  test "pass with valid scale constraint" do
    valid_attrs = @valid_attrs |> Map.put(:scaled_amount, 12.34)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "fail with invalid scale constraint" do
    # 3 decimal places (12.345) should fail for scale: 2
    invalid_attrs = @valid_attrs |> Map.put(:scaled_amount, 12.345)

    assert {:error, %Ash.Error.Invalid{}} =
             Balance
             |> Ash.Changeset.for_create(:create, invalid_attrs)
             |> Ash.create()
  end

  test "allow arbitrary scale when set to :arbitrary" do
    valid_attrs =
      @valid_attrs |> Map.put(:arbitrary_scale_amount, Decimal.new("123.123456789012345"))

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "scale validation with different decimal formats" do
    # Test that 12.34 (2 decimal places) passes scale: 2
    valid_attrs = @valid_attrs |> Map.put(:scaled_amount, 12.34)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()

    # Test that 12.3 (1 decimal place) passes scale: 2
    valid_attrs = @valid_attrs |> Map.put(:scaled_amount, 12.3)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()

    # Test that 12 (0 decimal places) passes scale: 2
    valid_attrs = @valid_attrs |> Map.put(:scaled_amount, 12)

    assert Balance
           |> Ash.Changeset.for_create(:create, valid_attrs)
           |> Ash.create!()
  end

  test "scale validation with string input" do
    # Test direct type casting with scale constraint
    constraints = [scale: 2]

    # Valid cases
    assert {:ok, _} = cast_input("123", constraints)
    assert {:ok, _} = cast_input("12.3", constraints)
    assert {:ok, _} = cast_input("1.23", constraints)

    # Invalid cases (more than 2 decimal places)
    assert {:error, _} = cast_input("1.234", constraints)
    assert {:error, _} = cast_input("12.345", constraints)
  end

  test "scale constraint defaults to :arbitrary" do
    # Test that default scale allows any number of decimal places
    constraints = []

    assert {:ok, _} =
             cast_input("123.123456789012345678901234567890", constraints)
  end

  test "combining precision and scale constraints" do
    # Test combining both precision and scale constraints
    constraints = [precision: 5, scale: 2]

    # Valid: 123.45 has 5 significant digits and 2 decimal places
    assert {:ok, _} = cast_input("123.45", constraints)

    # Invalid: too many significant digits
    assert {:error, errors} = cast_input("123456.78", constraints)
    assert Enum.any?(errors, fn error -> error[:message] =~ "significant digits" end)

    # Invalid: too many decimal places
    assert {:error, errors} = cast_input("123.456", constraints)
    assert Enum.any?(errors, fn error -> error[:message] =~ "decimal places" end)
  end

  test "scale validation edge cases" do
    constraints = [scale: 3]

    # Zero decimal places should pass
    assert {:ok, _} = cast_input("123", constraints)

    # Exactly at the limit should pass
    assert {:ok, _} = cast_input("123.456", constraints)

    # Over the limit should fail
    assert {:error, _} = cast_input("123.4567", constraints)
  end

  test "precision constraint prevents atomic usage" do
    # Test that precision constraint prevents atomic casting
    constraints = [precision: 3]
    expr = Ash.Expr.expr(1 + 1)

    assert {:not_atomic, message} = Ash.Type.Decimal.cast_atomic(expr, constraints)
    assert message =~ "precision"
  end

  test "scale constraint prevents atomic usage" do
    # Test that scale constraint prevents atomic casting
    constraints = [scale: 2]
    expr = Ash.Expr.expr(1 + 1)

    assert {:not_atomic, message} = Ash.Type.Decimal.cast_atomic(expr, constraints)
    assert message =~ "scale"
  end

  test "both precision and scale constraints prevent atomic usage" do
    # Test that both constraints together prevent atomic casting
    constraints = [precision: 5, scale: 2]
    expr = Ash.Expr.expr(1 + 1)

    assert {:not_atomic, message} = Ash.Type.Decimal.cast_atomic(expr, constraints)
    assert message =~ "precision"
  end

  test "atomic casting works when no precision or scale constraints" do
    # Test that atomic casting works when only other constraints are present
    constraints = [min: Decimal.new("0"), max: Decimal.new("100")]
    expr = Ash.Expr.expr(1 + 1)

    assert {:atomic, _} = Ash.Type.Decimal.cast_atomic(expr, constraints)
  end

  defp cast_input(value, constraints) do
    with {:ok, value} <- Ash.Type.Decimal.cast_input(value, constraints) do
      Ash.Type.Decimal.apply_constraints(value, constraints)
    end
  end
end
