# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.GeneratorTest do
  # async: false because the capability-population describe blocks mutate
  # Application.put_env(:ash, :custom_expressions, ...) in setup.
  use ExUnit.Case, async: false

  describe "generate/1" do
    test "generates spec for otp_app" do
      assert {:ok, %Ash.Info.Manifest{} = spec} =
               Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      assert is_list(spec.resources)
      assert spec.resources != []
    end

    test "all resources have names and modules" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      for resource <- spec.resources do
        assert is_binary(resource.name)
        assert is_atom(resource.module)
        assert is_boolean(resource.embedded?)
        assert is_map(resource.fields)
        assert is_map(resource.relationships)
      end

      assert is_list(spec.entrypoints)
      assert spec.entrypoints != []
    end

    test "includes Todo resource" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      todo = Enum.find(spec.resources, &(&1.module == Ash.Test.Manifest.Todo))
      assert todo != nil
      assert todo.name == "Todo"

      # Should have fields
      assert Map.has_key?(todo.fields, :id)
      assert Map.has_key?(todo.fields, :title)
    end

    test "includes User resource" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      user = Enum.find(spec.resources, &(&1.module == Ash.Test.Manifest.User))
      assert user != nil
    end

    test "with action filter narrows entrypoints to specified actions" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :read}]
        )

      assert Enum.any?(spec.entrypoints, fn e ->
               e.resource == Ash.Test.Manifest.Todo and e.action.name == :read
             end)

      # Only one entrypoint for Todo :read
      assert length(spec.entrypoints) == 1
    end

    test "with action filter, reachable resources have no entrypoints" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :read}]
        )

      # User is reachable via Todo's belongs_to but was not explicitly listed
      user = Enum.find(spec.resources, &(&1.module == Ash.Test.Manifest.User))
      assert user != nil
      refute Enum.any?(spec.entrypoints, &(&1.resource == Ash.Test.Manifest.User))
    end

    test "resources have properly resolved field types" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      todo = Enum.find(spec.resources, &(&1.module == Ash.Test.Manifest.Todo))
      assert todo.fields[:title].type.kind == :string
    end

    test "named type fields use type_ref, spec.types has full definitions" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :read}]
        )

      todo = Enum.find(spec.resources, &(&1.module == Ash.Test.Manifest.Todo))

      # Status field should be a type_ref inline
      status_type = todo.fields[:status].type
      assert status_type.kind == :type_ref
      assert status_type.module == Ash.Test.Manifest.Todo.Status

      # spec.types should have the full definition
      status_def = Enum.find(spec.types, &(&1.module == Ash.Test.Manifest.Todo.Status))
      assert status_def != nil
      assert status_def.kind == :enum
      assert :pending in status_def.values
    end

    test "resources are sorted by module name" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      module_names = Enum.map(spec.resources, &Module.split(&1.module))
      assert module_names == Enum.sort(module_names)
    end
  end

  describe "private actions" do
    test "are excluded from entrypoints by default" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      refute Enum.any?(spec.entrypoints, fn e ->
               e.resource == Ash.Test.Manifest.Todo and
                 e.action.name == :internal_reconcile
             end)
    end

    test "are excluded even when explicitly requested via action_entrypoints" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :internal_reconcile}]
        )

      refute Enum.any?(spec.entrypoints, fn e ->
               e.action.name == :internal_reconcile
             end)
    end

    test "are included when :include_private_actions? is true" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          include_private_actions?: true
        )

      assert Enum.any?(spec.entrypoints, fn e ->
               e.resource == Ash.Test.Manifest.Todo and
                 e.action.name == :internal_reconcile
             end)
    end

    test ":include_private_actions? works alongside explicit action_entrypoints" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :internal_reconcile}],
          include_private_actions?: true
        )

      assert [%{action: %{name: :internal_reconcile}}] = spec.entrypoints
    end
  end

  describe "overrides" do
    # Use NoRelationshipsResource as a narrow entrypoint — it has no relationships
    # and only primitive fields (id, name), so its reachable set is minimal.
    @narrow_entrypoints [{Ash.Test.Manifest.NoRelationshipsResource, :read}]

    test "always_resources forces inclusion of unreachable resources" do
      {:ok, spec_without} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: @narrow_entrypoints
        )

      refute Enum.any?(spec_without.resources, &(&1.module == Ash.Test.Manifest.User))

      {:ok, spec_with} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: @narrow_entrypoints,
          overrides: [always: [resources: [Ash.Test.Manifest.User]]]
        )

      user = Enum.find(spec_with.resources, &(&1.module == Ash.Test.Manifest.User))
      assert user != nil
      # Always-resources have no entrypoints
      refute Enum.any?(spec_with.entrypoints, &(&1.resource == Ash.Test.Manifest.User))
    end

    test "always_resources also discovers their field types via reachability" do
      {:ok, spec} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: @narrow_entrypoints,
          overrides: [always: [resources: [Ash.Test.Manifest.NotExposed]]]
        )

      # NotExposed has a belongs_to :todo relationship, so Todo should also be reachable
      assert Enum.any?(spec.resources, &(&1.module == Ash.Test.Manifest.Todo))
    end

    test "always_types forces inclusion of standalone types" do
      {:ok, spec_without} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: @narrow_entrypoints
        )

      type_modules_without = Enum.map(spec_without.types, & &1.module)

      {:ok, spec_with} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: @narrow_entrypoints,
          overrides: [always: [types: [Ash.Test.Manifest.Todo.Status]]]
        )

      type_modules_with = Enum.map(spec_with.types, & &1.module)

      refute Ash.Test.Manifest.Todo.Status in type_modules_without
      assert Ash.Test.Manifest.Todo.Status in type_modules_with
    end

    test "empty overrides has no effect" do
      {:ok, spec_without} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :read}]
        )

      {:ok, spec_with} =
        Ash.Info.Manifest.generate(
          otp_app: :ash_manifest_test,
          action_entrypoints: [{Ash.Test.Manifest.Todo, :read}],
          overrides: []
        )

      assert length(spec_without.resources) == length(spec_with.resources)
      assert length(spec_without.types) == length(spec_with.types)
    end
  end

  describe "generate/1 populates filter/sort capabilities" do
    setup do
      original = Application.get_env(:ash, :custom_expressions, [])

      Application.put_env(:ash, :custom_expressions, [
        Ash.Test.Expressions.JaroDistance
      ])

      on_exit(fn -> Application.put_env(:ash, :custom_expressions, original) end)

      :ok
    end

    test "the manifest carries a populated filter_capabilities" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      assert %Ash.Info.Manifest.FilterCapabilities{} = manifest.filter_capabilities
      assert manifest.filter_capabilities.operators != []
      assert manifest.filter_capabilities.functions != []
    end

    test "the manifest includes registered custom expressions" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      assert Enum.any?(manifest.filter_capabilities.custom_expressions, fn ce ->
               ce.module == Ash.Test.Expressions.JaroDistance
             end)
    end

    test "the manifest carries a populated sort_capabilities" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      assert %Ash.Info.Manifest.SortCapabilities{directions: directions} =
               manifest.sort_capabilities

      assert :asc in directions
      assert :desc_nils_last in directions
    end
  end

  describe "per-field operator/function resolution invariants" do
    test "every field's filter_operators is a subset of predicate_operators" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      catalog_predicate_ops = MapSet.new(manifest.filter_capabilities.predicate_operators)

      for resource <- manifest.resources, {_name, field} <- resource.fields do
        if field.filter_operators do
          field_op_names = MapSet.new(field.filter_operators, & &1.name)

          assert MapSet.subset?(field_op_names, catalog_predicate_ops),
                 "field #{resource.name}.#{field.name} has operators not in predicate_operators: #{inspect(MapSet.difference(field_op_names, catalog_predicate_ops))}"
        end
      end
    end

    test "every field's filter_functions is a subset of predicate_functions" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      catalog_predicate_fns = MapSet.new(manifest.filter_capabilities.predicate_functions)

      for resource <- manifest.resources, {_name, field} <- resource.fields do
        if field.filter_functions do
          field_fn_names = MapSet.new(field.filter_functions, & &1.name)

          assert MapSet.subset?(field_fn_names, catalog_predicate_fns),
                 "field #{resource.name}.#{field.name} has functions not in predicate_functions"
        end
      end
    end
  end
end
