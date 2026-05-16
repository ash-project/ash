# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.ResourceBuilderTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.{Field, Relationship, Resource}
  alias Ash.Info.Manifest.Generator.ResourceBuilder

  describe "build/2" do
    test "builds a resource from Todo" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)
      assert %Resource{} = resource
      assert resource.name == "Todo"
      assert resource.module == Ash.Test.Manifest.Todo
      assert resource.embedded? == false
      assert :id in resource.primary_key
    end

    test "includes public attributes as fields" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      assert Map.has_key?(resource.fields, :title)
      assert Map.has_key?(resource.fields, :description)
      assert Map.has_key?(resource.fields, :completed)
      assert Map.has_key?(resource.fields, :status)
    end

    test "marks attribute field properties correctly" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)
      title_field = resource.fields[:title]

      assert %Field{} = title_field
      assert title_field.kind == :attribute
      assert title_field.allow_nil? == false
      assert title_field.primary_key? == false
    end

    test "includes id as primary key field" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)
      id_field = resource.fields[:id]

      assert %Field{} = id_field
      assert id_field.primary_key? == true
      assert id_field.type.kind == :uuid
    end

    test "includes calculations as fields" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)
      calc_fields = resource.fields |> Map.values() |> Enum.filter(&(&1.kind == :calculation))

      calc_names = Enum.map(calc_fields, & &1.name)
      assert :is_overdue in calc_names
      assert :days_until_due in calc_names
    end

    test "calculation fields include arguments" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      self_calc = resource.fields[:self]
      assert self_calc != nil
      assert self_calc.kind == :calculation
      assert is_list(self_calc.arguments)
      assert self_calc.arguments != []

      arg_names = Enum.map(self_calc.arguments, & &1.name)
      assert :prefix in arg_names
    end

    test "includes aggregates as fields" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)
      agg_fields = resource.fields |> Map.values() |> Enum.filter(&(&1.kind == :aggregate))

      agg_names = Enum.map(agg_fields, & &1.name)
      assert :comment_count in agg_names
      assert :has_comments in agg_names
    end

    test "aggregate fields have aggregate_kind" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      comment_count = resource.fields[:comment_count]
      assert comment_count.aggregate_kind == :count
    end

    test "includes relationships" do
      assert Map.has_key?(ResourceBuilder.build(Ash.Test.Manifest.Todo).relationships, :user)
      assert Map.has_key?(ResourceBuilder.build(Ash.Test.Manifest.Todo).relationships, :comments)
    end

    test "relationship properties are correct" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      user_rel = resource.relationships[:user]
      assert %Relationship{} = user_rel
      assert user_rel.type == :belongs_to
      assert user_rel.cardinality == :one
      assert user_rel.destination == Ash.Test.Manifest.User

      comments_rel = resource.relationships[:comments]
      assert %Relationship{} = comments_rel
      assert comments_rel.type == :has_many
      assert comments_rel.cardinality == :many
    end

    test "builds embedded resource" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.TodoMetadata)
      assert resource.embedded? == true
    end

    test "builds resource with multitenancy" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.OrgTodo)

      if resource.multitenancy do
        assert is_atom(resource.multitenancy.strategy)
      end
    end
  end
end
