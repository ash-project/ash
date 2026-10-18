# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.ContainsTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  alias Ash.Query.Function.Contains

  test "returns true when string contains the substring" do
    assert eval!(expr(contains("foobar", "oba")))
  end

  test "returns false when string does not contain the substring" do
    refute eval!(expr(contains("foobar", "baz")))
  end

  test "case-insensitive on the left" do
    assert eval!(expr(contains(^%Ash.CiString{string: "FooBar"}, "oba")))
  end

  test "case-insensitive on the right" do
    assert eval!(expr(contains("FooBar", ^%Ash.CiString{string: "OBA"})))
  end

  test "false on nil" do
    refute eval!(expr(contains(nil, "foo")))
    refute eval!(expr(contains("foo", nil)))
  end

  for left <- [Ash.Type.String, Ash.Type.CiString],
      right <- [Ash.Type.String, Ash.Type.CiString] do
    test "contains of #{inspect(left)} and #{inspect(right)} returns a boolean" do
      args = [typed_ref(unquote(left)), typed_ref(unquote(right))]

      assert {[{unquote(left), _}, {unquote(right), _}], {Ash.Type.Boolean, []}} =
               Ash.Expr.determine_types(Contains, args)
    end
  end

  defp typed_ref(type) do
    %Ash.Query.Ref{attribute: %{name: :field, type: type, constraints: []}}
  end
end
