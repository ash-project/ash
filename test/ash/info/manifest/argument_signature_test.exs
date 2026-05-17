# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.ArgumentSignatureTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.ArgumentSignature

  describe "from_ash_signature/1" do
    test "normalizes a builtin-atom signature" do
      assert %ArgumentSignature{
               args: [
                 %{kind: :concrete, builtin: :string, type_ref: nil, constraints: []},
                 %{kind: :concrete, builtin: :string, type_ref: nil, constraints: []}
               ]
             } = ArgumentSignature.from_ash_signature([:string, :string])
    end

    test "normalizes :same and :any sentinels" do
      assert %ArgumentSignature{
               args: [
                 %{kind: :same, builtin: nil, type_ref: nil, constraints: []},
                 %{kind: :any, builtin: nil, type_ref: nil, constraints: []}
               ]
             } = ArgumentSignature.from_ash_signature([:same, :any])
    end

    test "normalizes module-only entries" do
      assert %ArgumentSignature{
               args: [
                 %{kind: :concrete, builtin: nil, type_ref: Ash.Type.Integer, constraints: []}
               ]
             } = ArgumentSignature.from_ash_signature([Ash.Type.Integer])
    end

    test "normalizes {module, constraints} tuples" do
      assert %ArgumentSignature{
               args: [
                 %{
                   kind: :concrete,
                   builtin: nil,
                   type_ref: Ash.Type.String,
                   constraints: [min_length: 1]
                 }
               ]
             } = ArgumentSignature.from_ash_signature([{Ash.Type.String, [min_length: 1]}])
    end

    test "normalizes {:array, builtin} tuples as :array kind carrying the inner spec" do
      assert %ArgumentSignature{
               args: [
                 %{
                   kind: :array,
                   builtin: nil,
                   type_ref: nil,
                   constraints: [],
                   of: %{kind: :concrete, builtin: :integer}
                 }
               ]
             } = ArgumentSignature.from_ash_signature([{:array, :integer}])
    end

    test "normalizes {:array, :same} tuples as :array kind carrying :same" do
      assert %ArgumentSignature{
               args: [
                 %{
                   kind: :array,
                   of: %{kind: :same}
                 }
               ]
             } = ArgumentSignature.from_ash_signature([{:array, :same}])
    end

    test "normalizes nested {:array, {:array, t}} tuples recursively" do
      assert %ArgumentSignature{
               args: [
                 %{
                   kind: :array,
                   of: %{kind: :array, of: %{kind: :concrete, builtin: :integer}}
                 }
               ]
             } = ArgumentSignature.from_ash_signature([{:array, {:array, :integer}}])
    end

    test "wraps a shorthand atom signature into a one-arg signature" do
      # Ash's default `types/0` is `[:same, :any]`. The outer list is signatures;
      # each element of the outer list can itself be a bare atom (`:same`/`:any`)
      # representing a one-arg signature with that arg type.
      assert %ArgumentSignature{args: [%{kind: :same}]} =
               ArgumentSignature.from_ash_signature(:same)

      assert %ArgumentSignature{args: [%{kind: :any}]} =
               ArgumentSignature.from_ash_signature(:any)
    end
  end
end
