# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT
#
# Reproduces the list-shaped authorization workload from
# https://github.com/Grupa36/Proteos/issues/465 without data-layer access.
#
# Every record is checked against five update actions. The manager is authorized
# by a deterministic bypass; the self-service actor must evaluate the regular
# policy. All filter expressions resolve from the in-memory record and context,
# so this measures policy preparation/evaluation rather than queries or SAT.
#
# Run with:
#
#     mix run benchmarks/policy_can.exs

defmodule PolicyCanBenchmark.Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule PolicyCanBenchmark.HasPermission do
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts), do: "has permission #{inspect(opts[:permission])}"

  @impl true
  def match?(_actor, %{subject: %{context: %{role: %{permissions: permissions}}}}, opts) do
    opts[:permission] in permissions
  end
end

defmodule PolicyCanBenchmark.WorkItem do
  use Ash.Resource,
    domain: PolicyCanBenchmark.Domain,
    data_layer: :embedded,
    authorizers: [Ash.Policy.Authorizer]

  alias PolicyCanBenchmark.HasPermission

  actions do
    defaults [:read]

    read :capacity_planning

    update :start
    update :stop
    update :cancel
    update :block
    update :unblock
  end

  policies do
    bypass expr(lab_id == ^context([:membership, :lab_id])) do
      authorize_if {HasPermission, permission: :orders_manage}
    end

    bypass [expr(lab_id == ^context([:membership, :lab_id])), action(:capacity_planning)] do
      authorize_if {HasPermission, permission: :staff_read}
      authorize_if {HasPermission, permission: :staff_manage}
    end

    bypass [expr(lab_id == ^context([:membership, :lab_id])), action_type(:read)] do
      authorize_if {HasPermission, permission: :orders_read}
    end

    policy_group expr(lab_id == ^context([:membership, :lab_id])) do
      policy action_type(:read) do
        forbid_unless {HasPermission, permission: :orders_work_self}
        authorize_if expr(assigned_membership_id == ^context([:membership, :id]))
      end

      policy action([:start, :stop, :cancel, :block, :unblock]) do
        forbid_unless {HasPermission, permission: :orders_work_self}
        authorize_if expr(assigned_membership_id == ^context([:membership, :id]))
      end
    end
  end

  attributes do
    integer_primary_key :id, writable?: true
    attribute :lab_id, :integer, allow_nil?: false, public?: true
    attribute :assigned_membership_id, :integer, allow_nil?: false, public?: true
  end
end

alias PolicyCanBenchmark.WorkItem

actions = [:start, :stop, :cancel, :block, :unblock]

checks =
  for id <- 1..100,
      action <- actions do
    record =
      struct!(WorkItem,
        id: id,
        lab_id: 1,
        assigned_membership_id: 10
      )

    {record, action}
  end

manager_context = %{
  shared: %{
    membership: %{id: 20, lab_id: 1},
    role: %{permissions: [:orders_manage]}
  }
}

self_service_context = %{
  shared: %{
    membership: %{id: 10, lab_id: 1},
    role: %{permissions: [:orders_work_self]}
  }
}

can_all = fn context ->
  Enum.map(checks, fn check ->
    Ash.can?(check, nil,
      context: context,
      reuse_values?: true,
      run_queries?: false
    )
  end)
end

jobs = %{
  "manager bypass" => fn -> can_all.(manager_context) end,
  "self-service policy" => fn -> can_all.(self_service_context) end,
  "can_do_all" => fn ->
    Ash.can_do_all(checks, nil,
      context: self_service_context,
      reuse_values?: true,
      run_queries?: false
    )
  end
}

Logger.configure(level: :error)

Benchee.run(jobs,
  warmup: 1,
  time: 5,
  memory_time: 2
)
