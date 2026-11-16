# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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
      attribute :on_hand, :integer, constraints: [min: 0]
      attribute :price, :decimal
      attribute :name, :string
      attribute :due_at, :utc_datetime
      attribute :grace, :duration
      attribute :tags, {:array, :string}
    end
  end

  defmacrop sigil_SQL(query, _modifiers)
  defmacrop sigil_SQL({:<<>>, _, [binary]}, []) when is_binary(binary), do: binary

  describe "compile time constants" do
    test "aggregate kinds match `Ash.Query.Aggregate.kinds/0`" do
      assert Ash.Expr.aggregate_kinds() == Ash.Query.Aggregate.kinds()
    end

    test "operator symbols match `Ash.Query.Operator.operator_symbols/0`" do
      assert Ash.Expr.operator_symbols() == Ash.Query.Operator.operator_symbols() -- [:is_nil]
    end
  end

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

  describe "sigils" do
    test "are literal values" do
      assert expr(~D[2024-01-01]) == ~D[2024-01-01]
      assert expr(~U[2024-01-01 00:00:00Z]) == ~U[2024-01-01 00:00:00Z]
      assert expr(~w(a b)a) == [:a, :b]
      assert expr(~SQL"foo") == "foo"
      assert expr(~i"Foo") == Ash.CiString.new("Foo")
    end

    test "with interpolation are rejected" do
      assert_raise ArgumentError, ~r/sigils with interpolation/, fn ->
        Code.eval_quoted(
          quote do
            import Ash.Expr

            expr(~s"#{name}")
          end
        )
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

    test "intersects resolves {:array, :same} to match the {:array, :any} type" do
      left = %Ash.Query.Aggregate{
        name: :comment_ids,
        type: {:array, Ash.Type.UUID},
        constraints: [],
        kind: :list,
        relationship_path: [:comments],
        field: :id
      }

      right = ["some-uuid-string"]

      {types, _returns} =
        determine_types(Ash.Query.Function.Intersects, [left, right], :boolean)

      assert [{{:array, Ash.Type.UUID}, items: []}, {{:array, Ash.Type.UUID}, items: []}] = types
    end

    test "a long chain of the same operator resolves in linear time" do
      # Each operand used to be re-resolved once per candidate signature, so a
      # chain nested `d` deep cost `signature_count ^ d`. With the 16 signatures
      # that `+` declares for the duration overloads, an 8-term sum took ~83s and
      # anything longer never finished.
      count = fn name ->
        %Ash.Query.Aggregate{name: name, kind: :count, type: Ash.Type.Integer, constraints: []}
      end

      chain =
        Enum.reduce(2..40, count.(:c1), fn n, acc ->
          %Ash.Query.Operator.Basic.Plus{left: acc, right: count.(:"c#{n}")}
        end)

      {microseconds, result} = :timer.tc(fn -> Ash.Expr.determine_type(chain) end)

      assert {:ok, {Ash.Type.Integer, []}} = result
      assert microseconds < 1_000_000
    end

    test "untyped expressions are not claimed by a signature nothing supports" do
      {:ok, %op{left: left, right: right}} =
        expr(fragment("1") + fragment("2"))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[nil, nil], nil} = determine_types(op, [left, right])
    end

    test "literals give their types when nothing else does" do
      {:ok, %op{left: left, right: right}} =
        expr(if(fragment("true"), do: 1, else: 0) + if(fragment("true"), do: 10, else: 0))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[{Ash.Type.Integer, []}, {Ash.Type.Integer, []}], {Ash.Type.Integer, []}} =
               determine_types(op, [left, right])

      {:ok, %func{arguments: args}} =
        expr(if(fragment("true"), do: 1, else: 0))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[_, {Ash.Type.Integer, []}, {Ash.Type.Integer, []}], {Ash.Type.Integer, []}} =
               determine_types(func, args)
    end

    test "literals don't override a type the expression gives" do
      {:ok, %op{left: left, right: right}} =
        expr(type(fragment("1"), :decimal) == if(fragment("true"), do: 1, else: 0))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[{Ash.Type.Decimal, []}, {Ash.Type.Decimal, []}], {Ash.Type.Boolean, []}} =
               determine_types(op, [left, right])

      {:ok, %func{arguments: args}} =
        expr(if(fragment("true"), do: 1, else: 0))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[_, {Ash.Type.Decimal, []}, {Ash.Type.Decimal, []}], {Ash.Type.Decimal, []}} =
               determine_types(func, args, Ash.Type.Decimal)
    end

    test "an untyped expression still takes a signature that another operand supports" do
      {:ok, %op{left: left, right: right}} =
        expr(fragment("now()") + ^Duration.new!(day: 1))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[{Ash.Type.Duration, []}, {Ash.Type.Duration, []}], {Ash.Type.Duration, []}} =
               determine_types(op, [left, right])
    end

    test "an untyped expression still takes the only signature there is" do
      {:ok, %func{arguments: args}} =
        expr(string_length(fragment("'foo'")))
        |> Ash.Filter.hydrate_refs(%{})

      assert {[{Ash.Type.String, []}], {Ash.Type.Integer, []}} = determine_types(func, args)
    end

    test "typing every node from type trees agrees with determine_types/4" do
      exprs = [
        expr(on_hand + 1 + on_hand * 2 - 3),
        expr(price / 2 + on_hand),
        expr(if price > 100, do: price - 1, else: price + on_hand),
        expr(
          cond do
            is_nil(price) -> 0
            price >= 10 and on_hand > 1 -> price * on_hand
            true -> -price
          end
        ),
        expr(due_at + grace > now()),
        expr(due_at - grace),
        expr(name <> "-" <> string_downcase(name)),
        expr(fragment("? + 1", on_hand) + on_hand),
        expr("a" in tags),
        expr((price || 0) + type(on_hand, :decimal)),
        expr(if on_hand > 0, do: %{a: on_hand + 1, b: name}, else: %{a: 0, b: "none"}),
        expr(if(fragment("true"), do: 1, else: 0) + if(fragment("true"), do: 10, else: 0)),
        expr(type(fragment("1"), :decimal) == if(fragment("true"), do: 1, else: 0)),
        expr(fragment("1") + fragment("2")),
        expr(fragment("now()") + ^Duration.new!(day: 1))
      ]

      for expr <- exprs do
        {:ok, expr} = Ash.Filter.hydrate_refs(expr, %{resource: Resource})

        assert type_top_down(expr, nil) == type_top_down_with_trees(expr, nil),
               "typing #{inspect(expr)} from type trees gave different types"

        assert Ash.Expr.determine_type_tree(expr).type == Ash.Expr.determine_type(expr)
      end
    end

    test "typing a deep expression top down from type trees takes linear time" do
      on_hand = Ash.Resource.Info.attribute(Resource, :on_hand)

      chain =
        Enum.reduce(1..3000, %Ash.Query.Ref{attribute: on_hand, resource: Resource}, fn n, acc ->
          %Ash.Query.Operator.Basic.Plus{left: acc, right: n, operator: :+}
        end)

      # Without the trees, each node re-derives the types of its operands from their
      # whole subtrees, taking time quadratic in the depth: about 4.5 million nodes typed
      # here, against 6000 with them.
      {microseconds, types} = :timer.tc(fn -> type_top_down_with_trees(chain, nil) end)

      assert length(types) == 3000
      assert Enum.all?(types, &match?({_, {Ash.Type.Integer, _}}, &1))
      assert microseconds < 1_000_000
    end

    test "upsert_conflict/1 has the type of the attribute it refers to" do
      {:ok, conflict} =
        expr(upsert_conflict(:on_hand))
        |> Ash.Filter.hydrate_refs(%{resource: Resource})

      assert {:ok, {Ash.Type.Integer, constraints}} = Ash.Expr.determine_type(conflict)
      assert constraints[:min] == 0
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

  describe "operators over two literals" do
    test "a result that is not a struct" do
      assert eval!(expr(^1 + ^2)) == 3
    end

    test "a decimal result" do
      assert eval!(expr(^Decimal.new(1) + ^Decimal.new(2))) == Decimal.new(3)
    end

    test "a datetime result" do
      now = DateTime.utc_now()
      duration = Duration.new!(day: 7)

      assert eval!(expr(^now + ^duration)) == DateTime.shift(now, duration)
    end

    test "a date result" do
      today = Date.utc_today()
      duration = Duration.new!(day: 7)

      assert eval!(expr(^today + ^duration)) == Date.shift(today, duration)
    end

    test "a duration result" do
      duration = Duration.new!(day: 7)

      assert eval!(expr(^duration + ^duration)) == Duration.add(duration, duration)
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

  describe "walk_template" do
    test "walks into Ash.Query.Calculation structs to resolve arg references" do
      # This test verifies that walk_template enters Calculation structs.
      # Without the fix, Calculation structs are treated as opaque and returned unchanged,
      # leaving {:_arg, :q} unresolved (which becomes NULL in SQL).

      calc = calc(fragment("similarity(id, ?)", ^arg(:q)), type: :float)

      assert %Ash.Query.Call{name: :fragment, args: ["similarity(id, ?)", {:_arg, :q}]} =
               calc.opts[:expr]

      # Use the same mapper pattern that fill_template uses for {:_arg, field}
      args = %{q: "test_value"}

      mapper = fn
        {:_arg, field} ->
          case Map.fetch(args, field) do
            :error -> Map.get(args, to_string(field))
            {:ok, value} -> value
          end

        other ->
          other
      end

      filled_calc = Ash.Expr.walk_template(calc, mapper)

      # The arg reference should now be resolved to the actual value
      assert %Ash.Query.Call{name: :fragment, args: ["similarity(id, ?)", "test_value"]} =
               filled_calc.opts[:expr]
    end
  end

  # Types an expression the way a data layer compiling it does: from the top down,
  # calling `determine_types/4` at each operator and function call, and passing the
  # types it resolves for the operands down as their expected types.
  defp type_top_down(expr, known) do
    case operands(expr) do
      {:typed, mod, operands} ->
        {types, returns} = Ash.Expr.determine_types(mod, operands, known)
        [{types, returns} | type_operands(operands, types, returns, &type_top_down/2)]

      {:untyped, operands} ->
        Enum.flat_map(operands, &type_top_down(&1, nil))
    end
  end

  # The same, taking the types of each node's operands from type trees.
  defp type_top_down_with_trees(expr, known) do
    do_type_top_down_with_trees(Ash.Expr.determine_type_tree(expr), known)
  end

  defp do_type_top_down_with_trees(%Ash.Expr.TypeTree{expr: expr} = tree, known) do
    case operands(expr) do
      {:typed, mod, operands} ->
        operand_trees = tree.operands || Enum.map(operands, &Ash.Expr.determine_type_tree/1)

        {types, returns} =
          Ash.Expr.determine_types_with_operand_types(
            mod,
            operands,
            Enum.map(operand_trees, & &1.type),
            known
          )

        [
          {types, returns}
          | type_operands(operand_trees, types, returns, &do_type_top_down_with_trees/2)
        ]

      {:untyped, operands} ->
        Enum.flat_map(operands, &type_top_down_with_trees(&1, nil))
    end
  end

  defp type_operands(operands, types, returns, fun) do
    types = if is_list(types), do: types, else: []

    operands
    |> Enum.with_index()
    |> Enum.flat_map(fn {operand, index} -> fun.(operand, Enum.at(types, index) || returns) end)
  end

  defp operands(%Ash.Query.Function.Type{arguments: [expr | _]}), do: {:untyped, [expr]}
  defp operands(%mod{__predicate__?: _, arguments: arguments}), do: {:typed, mod, arguments}

  defp operands(%mod{__predicate__?: _, left: left, right: right}),
    do: {:typed, mod, [left, right]}

  defp operands(%Ash.Query.BooleanExpression{left: left, right: right}),
    do: {:untyped, [left, right]}

  defp operands(%Ash.Query.Not{expression: expression}), do: {:untyped, [expression]}

  defp operands(map) when is_map(map) and not is_struct(map),
    do: {:untyped, Map.values(map)}

  defp operands(_), do: {:untyped, []}
end
