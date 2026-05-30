# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.CompositeLoadTest do
  @moduledoc """
  Verifies that `Ash.Type.Map`, `Ash.Type.Keyword`, and `Ash.Type.Tuple`
  recurse through their declared `fields:` constraint when loaded — so
  a composite value containing a `:struct` with `instance_of:` a resource
  with field policies has those policies applied automatically (even
  when no nested load was requested for the inner struct).
  """
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule AdminCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    @impl true
    def describe(_), do: "actor is admin"

    @impl true
    def match?(%{admin: true}, _, _), do: true
    def match?(_, _, _), do: false
  end

  defmodule NestedDoc do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: :embedded,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id, writable?: true
      attribute :public_note, :string, public?: true
      attribute :admin_note, :string, public?: true
    end

    calculations do
      calculate :public_note_length, :integer, expr(string_length(public_note)) do
        public? true
      end
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

      create :create do
        primary? true
        accept [:public_note, :admin_note]
      end
    end
  end

  defp build_doc do
    %NestedDoc{
      id: Ash.UUID.generate(),
      public_note: "visible",
      admin_note: "secret"
    }
  end

  defp context(actor) do
    %{
      domain: Domain,
      actor: actor,
      tenant: nil,
      tracer: nil,
      authorize?: true
    }
  end

  describe "Ash.Type.Map" do
    test "can_load? returns true when a declared field is loadable" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]],
          count: [type: :integer]
        ]
      ]

      assert Ash.Type.can_load?(Ash.Type.Map, constraints)
    end

    test "can_load? returns false when no declared field is loadable" do
      constraints = [
        fields: [
          count: [type: :integer],
          label: [type: :string]
        ]
      ]

      refute Ash.Type.can_load?(Ash.Type.Map, constraints)
    end

    test "load auto-recurses into struct field even with empty load and scrubs forbidden attributes for non-admin" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [%{nested: build_doc()}]

      {:ok, [loaded]} =
        Ash.Type.load(Ash.Type.Map, values, nil, constraints, context(%{admin: false}))

      assert loaded.nested.public_note == "visible"
      assert %Ash.ForbiddenField{field: :admin_note} = loaded.nested.admin_note
    end

    test "load auto-recurses for admin actor and the field is visible" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [%{nested: build_doc()}]

      {:ok, [loaded]} =
        Ash.Type.load(Ash.Type.Map, values, nil, constraints, context(%{admin: true}))

      assert loaded.nested.admin_note == "secret"
    end

    test "batches the slice — a single Ash.Type.load call services all N maps" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = for _ <- 1..5, do: %{nested: build_doc()}

      {:ok, loaded} =
        Ash.Type.load(Ash.Type.Map, values, nil, constraints, context(%{admin: false}))

      assert length(loaded) == 5

      Enum.each(loaded, fn record ->
        assert %Ash.ForbiddenField{field: :admin_note} = record.nested.admin_note
      end)
    end

    test "preserves keys not declared in fields" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [%{nested: build_doc(), extra: "passthrough"}]

      {:ok, [loaded]} =
        Ash.Type.load(Ash.Type.Map, values, nil, constraints, context(%{admin: false}))

      assert loaded.extra == "passthrough"
    end
  end

  describe "Ash.Type.Keyword" do
    test "load auto-recurses through declared fields" do
      constraints = [
        fields: [
          nested: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [[nested: build_doc()]]

      {:ok, [loaded]} =
        Ash.Type.load(Ash.Type.Keyword, values, nil, constraints, context(%{admin: false}))

      assert %Ash.ForbiddenField{field: :admin_note} = loaded[:nested].admin_note
    end
  end

  describe "deeply nested type with explicit sub-load on inner resource" do
    # Outer is a `:map` with a `:struct, instance_of: NestedDoc` field.
    # We ask for `[inner: [:public_note_length]]` — a calc on the
    # inner resource. After the load we should have:
    #
    #   * the calc loaded on the inner struct,
    #   * the populated attributes scrubbed via the inner resource's
    #     field policies (so admin_note becomes %Ash.ForbiddenField{}
    #     for a non-admin),
    #   * the rest of the outer map's keys untouched.

    test "loads requested calc on inner resource AND scrubs forbidden attrs" do
      constraints = [
        fields: [
          inner: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [%{inner: build_doc()}]

      {:ok, [loaded]} =
        Ash.Type.load(
          Ash.Type.Map,
          values,
          [inner: [:public_note_length]],
          constraints,
          context(%{admin: false})
        )

      # Calc the caller asked for is loaded on the inner record.
      assert loaded.inner.public_note_length == String.length("visible")

      # Populated attributes that came back on the inner record were
      # scrubbed by NestedDoc's field policies.
      assert loaded.inner.public_note == "visible"
      assert %Ash.ForbiddenField{field: :admin_note} = loaded.inner.admin_note
    end

    test "admin sees admin_note alongside the requested calc" do
      constraints = [
        fields: [
          inner: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [%{inner: build_doc()}]

      {:ok, [loaded]} =
        Ash.Type.load(
          Ash.Type.Map,
          values,
          [inner: [:public_note_length]],
          constraints,
          context(%{admin: true})
        )

      assert loaded.inner.public_note_length == String.length("visible")
      assert loaded.inner.admin_note == "secret"
    end
  end

  describe "Ash.Type.Tuple" do
    test "load auto-recurses positionally through declared fields" do
      constraints = [
        fields: [
          label: [type: :string],
          doc: [type: :struct, constraints: [instance_of: NestedDoc]]
        ]
      ]

      values = [{"hello", build_doc()}]

      {:ok, [{label, doc}]} =
        Ash.Type.load(Ash.Type.Tuple, values, nil, constraints, context(%{admin: false}))

      assert label == "hello"
      assert %Ash.ForbiddenField{field: :admin_note} = doc.admin_note
    end
  end
end
