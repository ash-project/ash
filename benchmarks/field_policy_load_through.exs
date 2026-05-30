# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT
#
# Measures the overhead of the unconditional-load-through behavior at
# varying composite-type nesting depths.
#
# `load_through_attributes/7` routes every loadable calc value through
# `Ash.Type.load`, which recurses through Map/Keyword/Tuple `fields:`
# constraints and through `Ash.Type.Struct`'s `instance_of:`. Each
# level of nesting adds another pass that may apply field policies
# (when the leaf is a resource with policies) or just walks the
# structure to find out it has nothing to do.
#
# Run with:
#
#     mix run benchmarks/field_policy_load_through.exs
#
# Tweak the size with `N=…` (default 500).
#
# To compare against a "before" state, stash the relevant lib changes
# and rerun.

defmodule Domain do
  use Ash.Domain, validate_config_inclusion?: false

  resources do
    allow_unregistered? true
  end
end

defmodule AdminCheck do
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_), do: "actor is admin"

  @impl true
  def match?(%{admin: true}, _, _), do: true
  def match?(_, _, _), do: false
end

defmodule LeafWithPolicies do
  use Ash.Resource,
    domain: Domain,
    data_layer: :embedded,
    authorizers: [Ash.Policy.Authorizer]

  attributes do
    uuid_primary_key :id, writable?: true
    attribute :public_note, :string, public?: true
    attribute :admin_note, :string, public?: true
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

    field_policy :* do
      authorize_if always()
    end
  end

  actions do
    defaults [:read]
  end
end

defmodule LeafNoPolicies do
  use Ash.Resource,
    domain: Domain,
    data_layer: :embedded

  attributes do
    uuid_primary_key :id, writable?: true
    attribute :public_note, :string, public?: true
    attribute :admin_note, :string, public?: true
  end

  actions do
    defaults [:read]
  end
end

defmodule Container do
  use Ash.Resource,
    domain: Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true
  end

  calculations do
    # ---- Depth 0: direct struct -----------------------------------

    calculate :leaf_with_policies, :struct do
      public? true
      constraints instance_of: LeafWithPolicies

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %LeafWithPolicies{
            id: Ash.UUID.generate(),
            public_note: "visible",
            admin_note: "secret"
          }
        end)
      end
    end

    calculate :leaf_no_policies, :struct do
      public? true
      constraints instance_of: LeafNoPolicies

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %LeafNoPolicies{
            id: Ash.UUID.generate(),
            public_note: "visible",
            admin_note: "secret"
          }
        end)
      end
    end

    # ---- Depth 1: map -> struct -----------------------------------

    calculate :map1_with_policies, :map do
      public? true

      constraints fields: [
                    inner: [
                      type: :struct,
                      constraints: [instance_of: LeafWithPolicies]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            inner: %LeafWithPolicies{
              id: Ash.UUID.generate(),
              public_note: "visible",
              admin_note: "secret"
            }
          }
        end)
      end
    end

    calculate :map1_no_policies, :map do
      public? true

      constraints fields: [
                    inner: [
                      type: :struct,
                      constraints: [instance_of: LeafNoPolicies]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            inner: %LeafNoPolicies{
              id: Ash.UUID.generate(),
              public_note: "visible",
              admin_note: "secret"
            }
          }
        end)
      end
    end

    # ---- Depth 2: map -> map -> struct ----------------------------

    calculate :map2_with_policies, :map do
      public? true

      constraints fields: [
                    middle: [
                      type: :map,
                      constraints: [
                        fields: [
                          inner: [
                            type: :struct,
                            constraints: [instance_of: LeafWithPolicies]
                          ]
                        ]
                      ]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            middle: %{
              inner: %LeafWithPolicies{
                id: Ash.UUID.generate(),
                public_note: "visible",
                admin_note: "secret"
              }
            }
          }
        end)
      end
    end

    calculate :map2_no_policies, :map do
      public? true

      constraints fields: [
                    middle: [
                      type: :map,
                      constraints: [
                        fields: [
                          inner: [
                            type: :struct,
                            constraints: [instance_of: LeafNoPolicies]
                          ]
                        ]
                      ]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            middle: %{
              inner: %LeafNoPolicies{
                id: Ash.UUID.generate(),
                public_note: "visible",
                admin_note: "secret"
              }
            }
          }
        end)
      end
    end

    # ---- Depth 3: map -> map -> map -> struct ---------------------

    calculate :map3_with_policies, :map do
      public? true

      constraints fields: [
                    outer: [
                      type: :map,
                      constraints: [
                        fields: [
                          middle: [
                            type: :map,
                            constraints: [
                              fields: [
                                inner: [
                                  type: :struct,
                                  constraints: [instance_of: LeafWithPolicies]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            outer: %{
              middle: %{
                inner: %LeafWithPolicies{
                  id: Ash.UUID.generate(),
                  public_note: "visible",
                  admin_note: "secret"
                }
              }
            }
          }
        end)
      end
    end

    calculate :map3_no_policies, :map do
      public? true

      constraints fields: [
                    outer: [
                      type: :map,
                      constraints: [
                        fields: [
                          middle: [
                            type: :map,
                            constraints: [
                              fields: [
                                inner: [
                                  type: :struct,
                                  constraints: [instance_of: LeafNoPolicies]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ ->
          %{
            outer: %{
              middle: %{
                inner: %LeafNoPolicies{
                  id: Ash.UUID.generate(),
                  public_note: "visible",
                  admin_note: "secret"
                }
              }
            }
          }
        end)
      end
    end

    # ---- Pure primitive composite (no resource anywhere) ----------

    calculate :pure_map, :map do
      public? true

      constraints fields: [
                    n: [type: :integer],
                    s: [type: :string]
                  ]

      calculation fn records, _context ->
        Enum.map(records, fn _ -> %{n: 1, s: "x"} end)
      end
    end
  end

  actions do
    defaults [:read]

    create :create do
      primary? true
      accept [:name]
    end
  end
end

# Seed N containers.
n = String.to_integer(System.get_env("N") || "500")

IO.puts("Seeding #{n} containers...")

for i <- 1..n do
  Container
  |> Ash.Changeset.for_create(:create, %{name: "C#{i}"})
  |> Ash.create!()
end

Logger.configure(level: :error)

actor = %{admin: false}

queries = %{
  "control (no calc load)" => Container |> Ash.Query.for_read(:read),
  "depth 0: leaf_no_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:leaf_no_policies]),
  "depth 0: leaf_with_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:leaf_with_policies]),
  "depth 1: map1_no_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map1_no_policies]),
  "depth 1: map1_with_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map1_with_policies]),
  "depth 2: map2_no_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map2_no_policies]),
  "depth 2: map2_with_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map2_with_policies]),
  "depth 3: map3_no_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map3_no_policies]),
  "depth 3: map3_with_policies" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:map3_with_policies]),
  "pure_map (no resource refs anywhere)" =>
    Container |> Ash.Query.for_read(:read) |> Ash.Query.load([:pure_map])
}

# Warmup all queries once so the first call's compilation isn't measured.
for {_, q} <- queries, do: Ash.read!(q, actor: actor)

scenarios =
  for {name, q} <- queries, into: %{} do
    {"N=#{n} | #{name}", fn -> Ash.read!(q, actor: actor) end}
  end

Benchee.run(scenarios,
  warmup: 1,
  time: 5,
  memory_time: 2
)
