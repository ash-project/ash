defmodule Ash.DataLayer.Mnesia.MatchSpecTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.DataLayer.Mnesia.MatchSpec
  alias Ash.Query.BooleanExpression
  alias Ash.Query.Not
  alias Ash.Query.Ref

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    IsNil,
    LessThan,
    LessThanOrEqual,
    NotEq
  }

  # Helper to create a simple field reference
  defp ref(name) do
    %Ref{
      attribute: name,
      relationship_path: [],
      resource: nil
    }
  end

  # Sample field map for testing
  defp field_map do
    %{
      id: :"$1",
      name: :"$2",
      age: :"$3",
      email: :"$4"
    }
  end

  describe "parse/2 - equality operators" do
    test "parses Eq operator" do
      expr = %Eq{left: ref(:name), right: "John"}

      assert {:ok, {:==, :"$2", "John"}} = MatchSpec.parse(expr, field_map())
    end

    test "parses NotEq operator" do
      expr = %NotEq{left: ref(:age), right: 25}

      assert {:ok, {:"=/=", :"$3", 25}} = MatchSpec.parse(expr, field_map())
    end

    test "parses IsNil operator" do
      expr = %IsNil{left: ref(:email)}

      assert {:ok, {:==, :"$4", nil}} = MatchSpec.parse(expr, field_map())
    end
  end

  describe "parse/2 - comparison operators" do
    test "parses GreaterThan operator" do
      expr = %GreaterThan{left: ref(:age), right: 18}

      assert {:ok, {:>, :"$3", 18}} = MatchSpec.parse(expr, field_map())
    end

    test "parses GreaterThanOrEqual operator" do
      expr = %GreaterThanOrEqual{left: ref(:age), right: 21}

      assert {:ok, {:>=, :"$3", 21}} = MatchSpec.parse(expr, field_map())
    end

    test "parses LessThan operator" do
      expr = %LessThan{left: ref(:age), right: 65}

      assert {:ok, {:<, :"$3", 65}} = MatchSpec.parse(expr, field_map())
    end

    test "parses LessThanOrEqual operator" do
      expr = %LessThanOrEqual{left: ref(:age), right: 100}

      assert {:ok, {:<=, :"$3", 100}} = MatchSpec.parse(expr, field_map())
    end
  end

  describe "parse/2 - boolean expressions" do
    test "parses AND expression" do
      left = %Eq{left: ref(:name), right: "John"}
      right = %GreaterThan{left: ref(:age), right: 18}
      expr = %BooleanExpression{op: :and, left: left, right: right}

      assert {:ok, {:andalso, {:==, :"$2", "John"}, {:>, :"$3", 18}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses OR expression" do
      left = %Eq{left: ref(:name), right: "John"}
      right = %Eq{left: ref(:name), right: "Jane"}
      expr = %BooleanExpression{op: :or, left: left, right: right}

      assert {:ok, {:orelse, {:==, :"$2", "John"}, {:==, :"$2", "Jane"}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses nested AND/OR expressions" do
      # (name == "John" AND age > 18) OR (name == "Jane" AND age > 21)
      left_and = %BooleanExpression{
        op: :and,
        left: %Eq{left: ref(:name), right: "John"},
        right: %GreaterThan{left: ref(:age), right: 18}
      }

      right_and = %BooleanExpression{
        op: :and,
        left: %Eq{left: ref(:name), right: "Jane"},
        right: %GreaterThan{left: ref(:age), right: 21}
      }

      expr = %BooleanExpression{op: :or, left: left_and, right: right_and}

      assert {:ok,
              {:orelse, {:andalso, {:==, :"$2", "John"}, {:>, :"$3", 18}},
               {:andalso, {:==, :"$2", "Jane"}, {:>, :"$3", 21}}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses NOT expression" do
      inner = %Eq{left: ref(:name), right: "John"}
      expr = %Not{expression: inner}

      assert {:ok, {:not, {:==, :"$2", "John"}}} = MatchSpec.parse(expr, field_map())
    end

    test "parses complex NOT with AND" do
      # NOT (name == "John" AND age > 18)
      inner = %BooleanExpression{
        op: :and,
        left: %Eq{left: ref(:name), right: "John"},
        right: %GreaterThan{left: ref(:age), right: 18}
      }

      expr = %Not{expression: inner}

      assert {:ok, {:not, {:andalso, {:==, :"$2", "John"}, {:>, :"$3", 18}}}} =
               MatchSpec.parse(expr, field_map())
    end
  end

  describe "parse/2 - In operator" do
    test "parses In with single value" do
      expr = %In{left: ref(:name), right: ["John"]}

      assert {:ok, {:==, :"$2", "John"}} = MatchSpec.parse(expr, field_map())
    end

    test "parses In with multiple values" do
      expr = %In{left: ref(:name), right: ["John", "Jane", "Bob"]}

      assert {:ok,
              {:orelse, {:orelse, {:==, :"$2", "John"}, {:==, :"$2", "Jane"}},
               {:==, :"$2", "Bob"}}} = MatchSpec.parse(expr, field_map())
    end

    test "parses In with empty list" do
      expr = %In{left: ref(:name), right: []}

      assert {:ok, false} = MatchSpec.parse(expr, field_map())
    end

    test "parses In with numeric values" do
      expr = %In{left: ref(:age), right: [25, 30, 35]}

      assert {:ok, {:orelse, {:orelse, {:==, :"$3", 25}, {:==, :"$3", 30}}, {:==, :"$3", 35}}} =
               MatchSpec.parse(expr, field_map())
    end
  end

  describe "parse/2 - error handling" do
    test "returns error for unknown field" do
      expr = %Eq{left: ref(:unknown_field), right: "value"}

      assert {:error, "Unknown field: unknown_field"} = MatchSpec.parse(expr, field_map())
    end

    test "returns error for relationship path" do
      ref_with_path = %Ref{
        attribute: :name,
        relationship_path: [:profile],
        resource: nil
      }

      expr = %Eq{left: ref_with_path, right: "value"}

      assert {:error, "Relationship traversal not supported in Mnesia matchspecs"} =
               MatchSpec.parse(expr, field_map())
    end

    test "returns error for unsupported expression" do
      unsupported = %{__struct__: :UnsupportedOperator}

      assert {:error, error_msg} = MatchSpec.parse(unsupported, field_map())
      assert error_msg =~ "Unsupported filter expression"
    end

    test "propagates error through AND expression" do
      left = %Eq{left: ref(:unknown), right: "John"}
      right = %GreaterThan{left: ref(:age), right: 18}
      expr = %BooleanExpression{op: :and, left: left, right: right}

      assert {:error, "Unknown field: unknown"} = MatchSpec.parse(expr, field_map())
    end

    test "propagates error through OR expression" do
      left = %Eq{left: ref(:name), right: "John"}
      right = %Eq{left: ref(:unknown), right: "Jane"}
      expr = %BooleanExpression{op: :or, left: left, right: right}

      assert {:error, "Unknown field: unknown"} = MatchSpec.parse(expr, field_map())
    end

    test "propagates error through NOT expression" do
      inner = %Eq{left: ref(:unknown), right: "John"}
      expr = %Not{expression: inner}

      assert {:error, "Unknown field: unknown"} = MatchSpec.parse(expr, field_map())
    end

    test "propagates error through In expression" do
      expr = %In{left: ref(:unknown), right: ["John", "Jane"]}

      assert {:error, "Unknown field: unknown"} = MatchSpec.parse(expr, field_map())
    end
  end

  describe "parse/2 - edge cases" do
    test "parses comparison with nil value" do
      expr = %Eq{left: ref(:email), right: nil}

      assert {:ok, {:==, :"$4", nil}} = MatchSpec.parse(expr, field_map())
    end

    test "parses comparison with boolean value" do
      expr = %Eq{left: ref(:name), right: true}

      assert {:ok, {:==, :"$2", true}} = MatchSpec.parse(expr, field_map())
    end

    test "parses comparison with negative number" do
      expr = %GreaterThan{left: ref(:age), right: -5}

      assert {:ok, {:>, :"$3", -5}} = MatchSpec.parse(expr, field_map())
    end

    test "parses comparison with float" do
      expr = %LessThan{left: ref(:age), right: 25.5}

      assert {:ok, {:<, :"$3", 25.5}} = MatchSpec.parse(expr, field_map())
    end

    test "parses comparison with string containing special characters" do
      expr = %Eq{left: ref(:email), right: "test@example.com"}

      assert {:ok, {:==, :"$4", "test@example.com"}} = MatchSpec.parse(expr, field_map())
    end
  end

  describe "complex real-world scenarios" do
    test "parses age range filter" do
      # age >= 18 AND age <= 65
      expr = %BooleanExpression{
        op: :and,
        left: %GreaterThanOrEqual{left: ref(:age), right: 18},
        right: %LessThanOrEqual{left: ref(:age), right: 65}
      }

      assert {:ok, {:andalso, {:>=, :"$3", 18}, {:<=, :"$3", 65}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses name search with multiple options" do
      # name == "John" OR name == "Jane" OR name == "Bob"
      expr1 = %BooleanExpression{
        op: :or,
        left: %Eq{left: ref(:name), right: "John"},
        right: %Eq{left: ref(:name), right: "Jane"}
      }

      expr = %BooleanExpression{
        op: :or,
        left: expr1,
        right: %Eq{left: ref(:name), right: "Bob"}
      }

      assert {:ok,
              {:orelse, {:orelse, {:==, :"$2", "John"}, {:==, :"$2", "Jane"}},
               {:==, :"$2", "Bob"}}} = MatchSpec.parse(expr, field_map())
    end

    test "parses email verification filter" do
      # email != nil AND email != ""
      expr = %BooleanExpression{
        op: :and,
        left: %Not{expression: %IsNil{left: ref(:email)}},
        right: %NotEq{left: ref(:email), right: ""}
      }

      assert {:ok, {:andalso, {:not, {:==, :"$4", nil}}, {:"=/=", :"$4", ""}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses exclusion filter" do
      # NOT (name IN ["banned1", "banned2"])
      inner = %In{left: ref(:name), right: ["banned1", "banned2"]}
      expr = %Not{expression: inner}

      assert {:ok, {:not, {:orelse, {:==, :"$2", "banned1"}, {:==, :"$2", "banned2"}}}} =
               MatchSpec.parse(expr, field_map())
    end

    test "parses active users filter" do
      # (age >= 18 AND email != nil) OR name IN ["admin", "moderator"]
      left_expr = %BooleanExpression{
        op: :and,
        left: %GreaterThanOrEqual{left: ref(:age), right: 18},
        right: %Not{expression: %IsNil{left: ref(:email)}}
      }

      right_expr = %In{left: ref(:name), right: ["admin", "moderator"]}
      expr = %BooleanExpression{op: :or, left: left_expr, right: right_expr}

      assert {:ok,
              {:orelse, {:andalso, {:>=, :"$3", 18}, {:not, {:==, :"$4", nil}}},
               {:orelse, {:==, :"$2", "admin"}, {:==, :"$2", "moderator"}}}} =
               MatchSpec.parse(expr, field_map())
    end
  end

  describe "guard structure validation" do
    test "AND expressions create andalso tuples" do
      expr = %BooleanExpression{
        op: :and,
        left: %Eq{left: ref(:name), right: "test"},
        right: %Eq{left: ref(:age), right: 25}
      }

      {:ok, result} = MatchSpec.parse(expr, field_map())
      assert elem(result, 0) == :andalso
      assert tuple_size(result) == 3
    end

    test "OR expressions create orelse tuples" do
      expr = %BooleanExpression{
        op: :or,
        left: %Eq{left: ref(:name), right: "test"},
        right: %Eq{left: ref(:age), right: 25}
      }

      {:ok, result} = MatchSpec.parse(expr, field_map())
      assert elem(result, 0) == :orelse
      assert tuple_size(result) == 3
    end

    test "comparison operators use correct mnesia operators" do
      comparisons = [
        {%Eq{left: ref(:age), right: 25}, :==},
        {%NotEq{left: ref(:age), right: 25}, :"=/="},
        {%GreaterThan{left: ref(:age), right: 25}, :>},
        {%GreaterThanOrEqual{left: ref(:age), right: 25}, :>=},
        {%LessThan{left: ref(:age), right: 25}, :<},
        {%LessThanOrEqual{left: ref(:age), right: 25}, :<=}
      ]

      for {expr, expected_op} <- comparisons do
        {:ok, result} = MatchSpec.parse(expr, field_map())
        assert elem(result, 0) == expected_op
        assert tuple_size(result) == 3
      end
    end

    test "matchspec variables are atoms with dollar prefix" do
      expr = %Eq{left: ref(:name), right: "test"}

      {:ok, {_op, var, _value}} = MatchSpec.parse(expr, field_map())
      assert is_atom(var)
      assert String.starts_with?(Atom.to_string(var), "$")
    end
  end
end
