# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ExprTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  defmodule Role do
    @moduledoc false

    use Ash.Type.Enum, values: [:admin, :user]
  end

  defmodule Resource do
    use Ash.Resource, domain: nil

    attributes do
      uuid_primary_key :id
      attribute :roles, {:array, Role}
    end
  end

  defmacrop sigil_SQL(query, _modifiers)
  defmacrop sigil_SQL({:<<>>, _, [binary]}, []) when is_binary(binary), do: binary

  describe "fragments" do
    test "allow pure binary sigils" do
      assert expr(fragment(~SQL"? > ?", 2, 1)) = expr(fragment("? > ?", 2, 1))
      assert expr(fragment(~S"? > ?", 2, 1)) = expr(fragment("? > ?", 2, 1))
      assert expr(fragment(~s"? > ?", 2, 1)) = expr(fragment("? > ?", 2, 1))

      injection_ast =
        quote do
          expr(fragment(~s"#{"sneaky"} ? > ?", 2, 1))
        end

      assert_raise RuntimeError, fn ->
        Code.eval_quoted(injection_ast)
      end
    end
  end

  describe "determine_types" do
    test "it determines the type of an if statement with complex values" do
      {:ok, %func{arguments: args}} =
        expr(
          if fragment("1") do
            string_downcase(type("foo", :string))
          else
            error(Foo, %{bar: "baz"})
          end
        )
        |> Ash.Filter.hydrate_refs(%{})

      assert {[{Ash.Type.Boolean, []}, {Ash.Type.String, []}, {Ash.Type.String, []}],
              {Ash.Type.String, []}} =
               determine_types(func, args, Ash.Type.String)
    end

    test "it determines the type of an if statement with semi-typed values" do
      {:ok, %func{arguments: args}} =
        expr(
          if type(fragment("\"george\""), Ash.Type.String, trim?: true, allow_empty?: false) ==
               "not allowed" do
            error(
              Ash.Error.Changes.InvalidAttribute,
              %{
                message: "must not equal %{value}",
                value: type("george", Ash.Type.String, trim?: true, allow_empty?: false),
                vars: %{value: "not allowed", field: :title},
                field: :title
              }
            )
          else
            id
          end
        )
        |> Ash.Filter.hydrate_refs(%{resource: Resource})

      # assert {[{Ash.Type.Boolean, []}, Ash.Type.String, Ash.Type.String], Ash.Type.String} =
      assert {[{Ash.Type.Boolean, []}, {Ash.Type.UUID, []}, {Ash.Type.UUID, []}],
              {Ash.Type.UUID, []}} =
               determine_types(func, args, Ash.Type.UUID)
    end

    test "it determines the type of an if statement with only the return type" do
      {:ok, %func{arguments: args}} =
        expr(
          if type(fragment("\"george\""), Ash.Type.String, trim?: true, allow_empty?: false) ==
               "not allowed" do
            error(
              Ash.Error.Changes.InvalidAttribute,
              %{
                message: "must not equal %{value}",
                value: type("george", Ash.Type.String, trim?: true, allow_empty?: false),
                vars: %{value: "not allowed", field: :title},
                field: :title
              }
            )
          else
            fragment("foo")
          end
        )
        |> Ash.Filter.hydrate_refs(%{resource: Resource})

      # assert {[{Ash.Type.Boolean, []}, Ash.Type.String, Ash.Type.String], Ash.Type.String} =
      assert {[{Ash.Type.Boolean, []}, {Ash.Type.UUID, []}, {Ash.Type.UUID, []}],
              {Ash.Type.UUID, []}} =
               determine_types(func, args, Ash.Type.UUID)
    end
  end

  describe "string interpolation" do
    test "pinned values can be used in interpolation" do
      var = "foo"
      expr = expr("#{^var}-#{^var}")
      assert eval!(expr) == "foo-foo"
    end
  end

  describe "type coercion" do
    test "integers are coerced to strings" do
      expr = expr("foo" <> type(2024, :string))
      assert eval!(expr) == "foo2024"
    end
  end

  describe "date expressions" do
    test "datetime diff expression" do
      now = DateTime.utc_now()
      tomorrow = DateTime.add(now, 1, :day)
      expr = expr(^tomorrow - ^now)
      assert eval!(expr) == 86_400
    end

    test "date diff expression" do
      now = Date.utc_today()
      tomorrow = Date.add(now, 1)
      expr = expr(^tomorrow - ^now)
      assert eval!(expr) == 1
    end
  end

  describe "rem expressions" do
    test "evaluates" do
      expr = expr(rem(1, 2) == 0)
      assert eval!(expr) == false
      expr = expr(rem(2, 2) == 0)
      assert eval!(expr) == true
    end
  end

  describe "case expressions" do
    test "raises error when using case expressions" do
      assert_raise ArgumentError, ~r/`case` expressions are not supported/, fn ->
        Code.eval_quoted(
          quote do
            import Ash.Expr

            expr(
              case :role do
                :principal -> 1
                :teacher -> 2
                :student -> 3
              end
            )
          end
        )
      end
    end
  end
end
