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

    test "aggregate allow_nil? reflects result nullability, not include_nil?" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      # :count, :exists, :list aggregates never return nil
      assert resource.fields[:comment_count].allow_nil? == false
      assert resource.fields[:helpful_comment_count].allow_nil? == false
      assert resource.fields[:has_comments].allow_nil? == false
      assert resource.fields[:comment_authors].allow_nil? == false
      assert resource.fields[:recent_comment_ids].allow_nil? == false
      assert resource.fields[:weighted_scores].allow_nil? == false

      # :first, :max, :avg, :sum aggregates can return nil on empty relationship
      assert resource.fields[:latest_comment_content].allow_nil? == true
      assert resource.fields[:highest_rating].allow_nil? == true
      assert resource.fields[:average_rating].allow_nil? == true
      assert resource.fields[:latest_comment_id].allow_nil? == true
      assert resource.fields[:total_weighted_score].allow_nil? == true
      assert resource.fields[:first_weighted_score].allow_nil? == true
      assert resource.fields[:max_weighted_score].allow_nil? == true
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

  describe "build/3 with filter capabilities — per-field operator resolution" do
    alias Ash.Info.Manifest.Generator.CapabilitiesBuilder

    setup do
      {filter_caps, _} = CapabilitiesBuilder.build()

      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo, [], filter_caps)
      {:ok, resource: resource}
    end

    test "filterable attribute fields carry filter_operators and filter_functions lists",
         %{resource: resource} do
      title = resource.fields[:title]
      assert is_list(title.filter_operators)
      assert is_list(title.filter_functions)
      assert Enum.any?(title.filter_operators, &(&1.name == :==))
    end

    test "non-filterable fields carry nil filter_operators / filter_functions",
         %{resource: resource} do
      Enum.each(resource.fields, fn {_name, field} ->
        if field.filterable? == false do
          assert field.filter_operators == nil
          assert field.filter_functions == nil
          assert field.filter_custom_expressions == nil
        end
      end)
    end

    test "filterable fields carry a list of filter_custom_expressions",
         %{resource: resource} do
      title = resource.fields[:title]
      assert is_list(title.filter_custom_expressions)
    end

    test "calculation fields get operator lists from their declared type",
         %{resource: resource} do
      # is_overdue is :boolean — gets eq/neq/in/is_nil
      is_overdue = resource.fields[:is_overdue]

      if is_overdue && is_overdue.filterable? do
        names = Enum.map(is_overdue.filter_operators, & &1.name)
        assert :== in names
        assert :is_nil in names
      end
    end

    test "aggregate fields get operator lists from their resolved type",
         %{resource: resource} do
      aggregates = resource.fields |> Map.values() |> Enum.filter(&(&1.kind == :aggregate))

      Enum.each(aggregates, fn agg ->
        if agg.filterable? do
          assert is_list(agg.filter_operators)
        end
      end)
    end

    test "filter_operators entries are %ApplicableOperator{} records with name + rhs",
         %{resource: resource} do
      title = resource.fields[:title]

      alias Ash.Info.Manifest.ApplicableOperator
      assert Enum.all?(title.filter_operators, &match?(%ApplicableOperator{}, &1))

      eq = Enum.find(title.filter_operators, &(&1.name == :==))
      assert eq.rhs == :same

      in_op = Enum.find(title.filter_operators, &(&1.name == :in))
      assert in_op.rhs == {:array, :same}
    end
  end

  describe "build/2 with no capabilities passed (backwards compat)" do
    test "filter_operators and filter_functions remain nil" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo, [])
      title = resource.fields[:title]
      assert title.filter_operators == nil
      assert title.filter_functions == nil
      assert title.filter_custom_expressions == nil
    end
  end

  describe "build/2 relationship filterable?/sortable? carry source flags" do
    test "relationships default to filterable? and sortable? true" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      user = resource.relationships[:user]
      assert user.filterable? == true
      assert user.sortable? == true

      comments = resource.relationships[:comments]
      assert comments.filterable? == true
      assert comments.sortable? == true
    end

    test "cardinality is exposed so consumers can derive filter/sort shape" do
      resource = ResourceBuilder.build(Ash.Test.Manifest.Todo)

      assert resource.relationships[:user].cardinality == :one
      assert resource.relationships[:comments].cardinality == :many
    end
  end
end
