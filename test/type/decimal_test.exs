defmodule Ash.Test.Type.DecimalTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

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
    end
  end

  @valid_attrs %{
    amount: 1.0,
    positive_amount: 1.0,
    negative_amount: -1.0
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
end
