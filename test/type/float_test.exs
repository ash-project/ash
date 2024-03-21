defmodule Ash.Test.Type.FloatTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

  defmodule Balance do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :amount, :float, allow_nil?: false
      attribute :positive_amount, :float, allow_nil?: false, constraints: [gt: 0]
      attribute :negative_amount, :float, allow_nil?: false, constraints: [lt: 0]
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Balance
    end
  end

  @valid_attrs %{
    amount: 1.0,
    positive_amount: 1.0,
    negative_amount: -1.0
  }

  test "pass with valid attrs" do
    assert Balance
           |> new(@valid_attrs)
           |> Api.create!()
  end

  test "fail with invalid positive_amount" do
    for positive_amount <- [0, -1] do
      invalid_attrs = @valid_attrs |> Map.put(:positive_amount, positive_amount)

      assert {:error, %Ash.Error.Invalid{}} =
               Balance
               |> new(invalid_attrs)
               |> Api.create()
    end
  end

  test "fail with invalid negative_amount" do
    for negative_amount <- [0, 1] do
      invalid_attrs = @valid_attrs |> Map.put(:negative_amount, negative_amount)

      assert {:error, %Ash.Error.Invalid{}} =
               Balance
               |> new(invalid_attrs)
               |> Api.create()
    end
  end
end
