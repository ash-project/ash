defmodule Ash.Test.Filter.SimplificationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  require Ash.Test.Helpers

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :read
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :points, :integer
      attribute :post_date, :date
    end
  end

  describe "date" do
    test "less than or equal to" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, post_date <= ^~D[2021-01-01])

      assert %Ash.Query.Operator.LessThan{
               operator: :<,
               right: ~D[2021-01-02]
             } = Ash.SatSolver.transform(Post, expr)
    end

    test "less than" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, post_date < ^~D[2021-01-01])

      assert %Ash.Query.Operator.LessThan{
               operator: :<,
               right: ~D[2021-01-01]
             } = Ash.SatSolver.transform(Post, expr)
    end

    test "greater than or equal to" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, post_date >= ^~D[2021-01-01])

      assert {:not,
              %Ash.Query.Operator.LessThan{
                operator: :<,
                right: ~D[2021-01-01]
              }} = Ash.SatSolver.transform(Post, expr)
    end

    test "greater than" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, post_date > ^~D[2021-01-01])

      assert {:not,
              %Ash.Query.Operator.LessThan{
                operator: :<,
                right: ~D[2021-01-02]
              }} = Ash.SatSolver.transform(Post, expr)
    end
  end

  describe "integer" do
    test "less than or equal to" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, points <= 1)

      assert %Ash.Query.Operator.LessThan{
               operator: :<,
               right: 2
             } = Ash.SatSolver.transform(Post, expr)
    end

    test "less than" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, points < 1)

      assert %Ash.Query.Operator.LessThan{
               operator: :<,
               right: 1
             } = Ash.SatSolver.transform(Post, expr)
    end

    test "greater than or equal to" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, points >= 1)

      assert {:not,
              %Ash.Query.Operator.LessThan{
                operator: :<,
                right: 1
              }} = Ash.SatSolver.transform(Post, expr)
    end

    test "greater than" do
      {:ok, expr} = Ash.Test.Helpers.hydrated_expr(Post, points > 1)

      assert {:not,
              %Ash.Query.Operator.LessThan{
                operator: :<,
                right: 2
              }} = Ash.SatSolver.transform(Post, expr)
    end
  end
end
