# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.GenericActionFieldPolicyTest do
  @moduledoc """
  Field policies are applied to generic action return values when the
  caller passes `load:` to `Ash.ActionInput.for_action/4` (or
  `Ash.ActionInput.load/2`). The action's post-run load dispatches
  through `Ash.Type.load`, which goes through the resource's read
  pipeline and scrubs forbidden fields on a per-record basis.
  """
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule AdminCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    @impl true
    def describe(_), do: "actor is admin"

    @impl true
    def match?(%{admin: true}, _ctx, _opts), do: true
    def match?(_, _, _), do: false
  end

  defmodule Approver do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :report_id, :uuid, allow_nil?: false, public?: true
      attribute :approver_id, :uuid, allow_nil?: false, public?: true
    end

    actions do
      defaults [:read]

      create :create do
        primary? true
        accept [:report_id, :approver_id]
      end
    end
  end

  defmodule Report do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :public_note, :string, public?: true
      attribute :admin_note, :string, public?: true
      attribute :private_note, :string, public?: true
      attribute :approval_note, :string, public?: true
      attribute :owner_id, :uuid, public?: true
    end

    relationships do
      has_many :approvers, Approver, public?: true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :admin_note do
        authorize_if AdminCheck
      end

      # Filter-check path: only the owner can see the private_note.
      field_policy :private_note do
        authorize_if expr(^actor(:id) == owner_id)
      end

      # `exists` references a relationship that won't be loaded on the
      # in-hand record — forces `eager_eval` to return `:unknown` so the
      # `Ash.load` fallback path actually runs.
      field_policy :approval_note do
        authorize_if expr(exists(approvers, approver_id == ^actor(:id)))
      end

      field_policy :* do
        authorize_if always()
      end
    end

    actions do
      defaults [:read]

      create :create do
        primary? true

        accept [
          :title,
          :public_note,
          :admin_note,
          :private_note,
          :approval_note,
          :owner_id
        ]
      end

      action :latest, :struct do
        constraints instance_of: __MODULE__

        run fn _input, _ctx ->
          case __MODULE__
               |> Ash.Query.sort(title: :asc)
               |> Ash.Query.limit(1)
               |> Ash.read(authorize?: false) do
            {:ok, [record]} -> {:ok, record}
            {:ok, []} -> {:error, "no record"}
            {:error, error} -> {:error, error}
          end
        end
      end

      action :all, {:array, :struct} do
        constraints items: [instance_of: __MODULE__]

        run fn _input, _ctx ->
          __MODULE__
          |> Ash.Query.sort(title: :asc)
          |> Ash.read(authorize?: false)
        end
      end
    end
  end

  @owner_id "11111111-1111-1111-1111-111111111111"
  @stranger_id "22222222-2222-2222-2222-222222222222"

  @approver_id "33333333-3333-3333-3333-333333333333"

  setup do
    {:ok, report} =
      Report
      |> Ash.Changeset.for_create(:create, %{
        title: "Q1",
        public_note: "visible",
        admin_note: "secret",
        private_note: "owner-only",
        approval_note: "approver-only",
        owner_id: @owner_id
      })
      |> Ash.create(actor: %{admin: true})

    {:ok, _} =
      Approver
      |> Ash.Changeset.for_create(:create, %{
        report_id: report.id,
        approver_id: @approver_id
      })
      |> Ash.create()

    :ok
  end

  describe "protected field metadata" do
    test "policy authorizer reports fields that may be scrubbed" do
      assert MapSet.new(Ash.Resource.Info.protected_fields(Report)) ==
               MapSet.new([:admin_note, :private_note, :approval_note])
    end
  end

  describe "single-record generic action return" do
    test "non-admin sees admin_note scrubbed when load: includes it" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{},
          actor: %{admin: false},
          load: [:title, :public_note, :admin_note]
        )
        |> Ash.run_action()

      assert report.title == "Q1"
      assert report.public_note == "visible"
      assert %Ash.ForbiddenField{field: :admin_note} = report.admin_note
    end

    test "admin sees admin_note when load: includes it" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{},
          actor: %{admin: true},
          load: [:title, :public_note, :admin_note]
        )
        |> Ash.run_action()

      assert report.admin_note == "secret"
    end

    test "non-admin still gets admin_note scrubbed even when not in load (already-populated attrs are reselected)" do
      # The post-action load calls `select_selected`, which adds any
      # already-populated attributes back into the read query's select.
      # That triggers field-policy calc injection for `admin_note` too,
      # so the scrub happens even though the caller didn't request it.
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{},
          actor: %{admin: false},
          load: [:title, :public_note]
        )
        |> Ash.run_action()

      assert report.title == "Q1"
      assert %Ash.ForbiddenField{field: :admin_note} = report.admin_note
    end
  end

  describe "list generic action return" do
    test "non-admin sees admin_note scrubbed on each record in the list" do
      {:ok, reports} =
        Report
        |> Ash.ActionInput.for_action(:all, %{},
          actor: %{admin: false},
          load: [:title, :public_note, :admin_note]
        )
        |> Ash.run_action()

      assert [report] = reports
      assert report.title == "Q1"
      assert %Ash.ForbiddenField{field: :admin_note} = report.admin_note
    end
  end

  describe "generic action return without load:" do
    test "non-admin gets admin_note scrubbed even when no load: is passed" do
      # When the action returns a resource record, the post-action load
      # runs unconditionally so field policies scrub already-populated
      # attributes — there is no way for a caller to accidentally skip
      # scrubbing by omitting `load:`.
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{admin: false})
        |> Ash.run_action()

      assert report.title == "Q1"
      assert report.public_note == "visible"
      assert %Ash.ForbiddenField{field: :admin_note} = report.admin_note
    end

    test "admin sees admin_note even when no load: is passed" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{admin: true})
        |> Ash.run_action()

      assert report.admin_note == "secret"
    end
  end

  describe "filter-check field policy via in-memory scrub (no load:)" do
    test "owner sees private_note when no load: is passed" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{id: @owner_id})
        |> Ash.run_action()

      assert report.private_note == "owner-only"
    end

    test "non-owner has private_note scrubbed when no load: is passed" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{id: @stranger_id})
        |> Ash.run_action()

      assert %Ash.ForbiddenField{field: :private_note} = report.private_note
    end
  end

  describe "field policies that can't be eagerly evaluated (e.g. exists)" do
    # Field policies referencing unloaded relationships, aggregates, or
    # calcs that need the data layer can't be resolved purely in memory.
    # `apply_field_level_auth/3` defers to `Ash.load` with
    # `reuse_values?: true` so the calc engine eagerly evaluates against
    # already-loaded values where possible and hits the data layer for
    # the rest (batched across the whole record set, not per record).

    test "approver sees approval_note when no load: is passed (Ash.load resolves exists)" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{id: @approver_id})
        |> Ash.run_action()

      assert report.approval_note == "approver-only"
    end

    test "non-approver has approval_note scrubbed when no load: is passed" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{id: @stranger_id})
        |> Ash.run_action()

      assert %Ash.ForbiddenField{field: :approval_note} = report.approval_note
    end

    test "approver sees approval_note when the field is in load:" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{},
          actor: %{id: @approver_id},
          load: [:approval_note]
        )
        |> Ash.run_action()

      assert report.approval_note == "approver-only"
    end

    test "non-approver has approval_note scrubbed when the field is in load:" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{},
          actor: %{id: @stranger_id},
          load: [:approval_note]
        )
        |> Ash.run_action()

      assert %Ash.ForbiddenField{field: :approval_note} = report.approval_note
    end
  end

  describe "load passed via Ash.run_action/2 opts" do
    test "non-admin sees admin_note scrubbed when load: is passed to run_action" do
      {:ok, report} =
        Report
        |> Ash.ActionInput.for_action(:latest, %{}, actor: %{admin: false})
        |> Ash.run_action(load: [:title, :admin_note], actor: %{admin: false})

      assert %Ash.ForbiddenField{field: :admin_note} = report.admin_note
    end
  end
end
