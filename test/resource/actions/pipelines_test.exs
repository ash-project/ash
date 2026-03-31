# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Actions.PipelinesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Info

  defmacrop defresource(name, do: body) do
    quote do
      defmodule unquote(name) do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :name, :string, public?: true
          attribute :score, :integer, public?: true, default: 0
          attribute :state, :atom, public?: true
        end

        unquote(body)
      end
    end
  end

  describe "full pipeline into CUD action" do
    setup do
      defresource FullCUD do
        pipelines do
          pipeline :full do
            change set_attribute(:score, 1)
            validate present(:name)
            prepare build(sort: [:name])
          end
        end

        actions do
          create :with_full do
            pipe_through [:full]
            accept [:name]
            change set_attribute(:state, :created)
          end
        end
      end

      %{action: Info.action(FullCUD, :with_full)}
    end

    test "injects pipeline changes and validations into changes", %{action: action} do
      assert [pipeline_change, pipeline_validation, action_change] = action.changes

      assert pipeline_change.change ==
               {Ash.Resource.Change.SetAttribute, value: 1, attribute: :score}

      {mod, opts} = pipeline_validation.validation
      assert mod == Ash.Resource.Validation.Present
      assert opts[:attributes] == [:name]

      assert action_change.change ==
               {Ash.Resource.Change.SetAttribute, value: :created, attribute: :state}
    end

    test "does not inject preparations into CUD action", %{action: action} do
      refute Enum.any?(action.changes, &match?(%Ash.Resource.Preparation{}, &1))
    end
  end

  describe "full pipeline into read action" do
    setup do
      defresource FullRead do
        pipelines do
          pipeline :full do
            change set_attribute(:score, 1)
            validate present(:name)
            prepare build(sort: [:name])
          end
        end

        actions do
          read :with_full do
            pipe_through [:full]
          end
        end
      end

      %{action: Info.action(FullRead, :with_full)}
    end

    test "injects preparations and validations into preparations", %{action: action} do
      assert [prep, validation] = action.preparations

      assert prep.preparation == {Ash.Resource.Preparation.Build, options: [sort: [:name]]}

      {mod, opts} = validation.validation
      assert mod == Ash.Resource.Validation.Present
      assert opts[:attributes] == [:name]
    end

    test "does not inject changes into read action", %{action: action} do
      refute Enum.any?(action.preparations, &match?(%Ash.Resource.Change{}, &1))
    end
  end

  describe "multiple pipelines ordering" do
    test "pipeline_a entities, then pipeline_b, then action's own" do
      defresource MultiPipeline do
        pipelines do
          pipeline :pipeline_a do
            change set_attribute(:score, 1)
          end

          pipeline :pipeline_b do
            change set_attribute(:state, :active)
          end
        end

        actions do
          create :multi do
            pipe_through [:pipeline_a, :pipeline_b]
            accept [:name]
            change set_attribute(:name, "from_action")
          end
        end
      end

      action = Info.action(MultiPipeline, :multi)
      [first, second, third] = action.changes

      assert first.change == {Ash.Resource.Change.SetAttribute, value: 1, attribute: :score}

      assert second.change ==
               {Ash.Resource.Change.SetAttribute, value: :active, attribute: :state}

      assert third.change ==
               {Ash.Resource.Change.SetAttribute, value: "from_action", attribute: :name}
    end
  end

  describe "pipe_through placement ordering" do
    test "preserves declaration order — change, pipe_through, change" do
      defresource PlacementBetween do
        pipelines do
          pipeline :p do
            change set_attribute(:score, 1)
          end
        end

        actions do
          create :ordered do
            accept [:name]
            change set_attribute(:state, :before)
            pipe_through [:p]
            change set_attribute(:name, "after")
          end
        end
      end

      action = Info.action(PlacementBetween, :ordered)
      [first, second, third] = action.changes

      assert first.change ==
               {Ash.Resource.Change.SetAttribute, value: :before, attribute: :state}

      assert second.change ==
               {Ash.Resource.Change.SetAttribute, value: 1, attribute: :score}

      assert third.change ==
               {Ash.Resource.Change.SetAttribute, value: "after", attribute: :name}
    end
  end

  describe "multiple pipe_through declarations" do
    test "each declaration's entities are appended in order with separate where" do
      defresource MultiPipeThrough do
        pipelines do
          pipeline :audit do
            change set_attribute(:score, 1)
          end

          pipeline :guard do
            change set_attribute(:state, :guarded)
          end
        end

        actions do
          create :guarded_audit do
            pipe_through [:audit]
            pipe_through [:guard], where: present(:name)
            accept [:name]
          end
        end
      end

      action = Info.action(MultiPipeThrough, :guarded_audit)
      assert length(action.changes) == 2

      [audit_change, guard_change] = action.changes
      assert audit_change.where == []
      assert length(guard_change.where) == 1
    end
  end

  describe "where conditional" do
    test "applies where to all pipeline entities" do
      defresource WhereConditional do
        pipelines do
          pipeline :guarded do
            change set_attribute(:score, 99)
            validate present(:state)
          end
        end

        actions do
          create :guarded_create do
            pipe_through [:guarded], where: present(:name)
            accept [:name]
          end
        end
      end

      action = Info.action(WhereConditional, :guarded_create)
      assert length(action.changes) == 2

      Enum.each(action.changes, fn entity ->
        assert length(entity.where) >= 1
      end)
    end

    test "prepends pipe_through where to entity's existing where" do
      defresource WhereMerge do
        pipelines do
          pipeline :already_guarded do
            change set_attribute(:score, 99), where: present(:state)
          end
        end

        actions do
          create :double_guard do
            pipe_through [:already_guarded], where: present(:name)
            accept [:name]
          end
        end
      end

      action = Info.action(WhereMerge, :double_guard)
      [change] = action.changes
      assert length(change.where) == 2
    end
  end

  describe "error cases" do
    test "raises error for unknown pipeline" do
      assert_raise Spark.Error.DslError, ~r/no pipeline named `nonexistent` exists/, fn ->
        defresource UnknownPipeline do
          actions do
            create :bad do
              pipe_through [:nonexistent]
              accept [:name]
            end
          end
        end
      end
    end

    test "raises error when validation does not support the action's subject type" do
      assert_raise Spark.Error.DslError, ~r/does not support/, fn ->
        defresource UnsupportedValidation do
          pipelines do
            pipeline :changeset_only do
              validate {Ash.Resource.Validation.DataOneOf, attribute: :name, values: ["a", "b"]}
            end
          end

          actions do
            read :bad_read do
              pipe_through [:changeset_only]
            end
          end
        end
      end
    end
  end

  describe "introspection" do
    test "Info.pipelines/1 and Info.pipeline/2" do
      defresource IntrospectPipelines do
        pipelines do
          pipeline :a do
            change set_attribute(:score, 1)
          end

          pipeline :b do
            prepare build(sort: [:name])
          end
        end

        actions do
          defaults [:read]
        end
      end

      pipelines = Info.pipelines(IntrospectPipelines)
      assert length(pipelines) == 2
      assert Enum.map(pipelines, & &1.name) |> Enum.sort() == [:a, :b]

      pipeline_a = Info.pipeline(IntrospectPipelines, :a)
      assert pipeline_a.name == :a
      assert length(pipeline_a.changes) == 1

      assert Info.pipeline(IntrospectPipelines, :nonexistent) == nil
    end
  end
end
