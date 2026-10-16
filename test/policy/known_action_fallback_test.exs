# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.KnownActionFallbackTest do
  use ExUnit.Case, async: true

  defmodule CountedCheck do
    use Ash.Policy.SimpleCheck

    def describe(options), do: "permission #{options[:permission]}"

    def simplify({_, options} = reference, _context) do
      send(self(), {:simplified_permission, options[:permission]})
      reference
    end

    def match?(actor, _context, options), do: Map.get(actor || %{}, options[:permission], false)
  end

  defmodule RuntimeInvariantCheck do
    use Ash.Policy.Check

    def describe(options), do: "runtime flag #{options[:expected]}"
    def strict_check(_, _, _), do: {:ok, :unknown}
    def check(_, records, _, _), do: records

    def conflicts?({__MODULE__, left}, {__MODULE__, right}, _context) do
      send(self(), :runtime_invariant_checked)
      left[:expected] != right[:expected]
    end

    def conflicts?(_, _, _), do: false
  end

  defmodule Resource do
    use Ash.Resource,
      domain: Ash.Test.Policy.KnownActionFallbackTest.Domain,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :secret, :string, public?: true
    end

    actions do
      defaults [:read, :create, :update, :destroy]
      read :bypass_read
      read :private_read, public?: false
    end

    policies do
      policy action_type(:read) do
        authorize_if {CountedCheck, permission: :resource_read}
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if {CountedCheck, permission: :resource_mutation}
      end
    end

    field_policies do
      field_policy :secret do
        authorize_if {CountedCheck, permission: :field_read}
      end

      field_policy :* do
        authorize_if always()
      end
    end
  end

  defmodule Domain do
    use Ash.Domain, extensions: [Ash.Policy.Authorizer]

    resources do
      resource Resource
    end

    policies do
      bypass action(:bypass_read) do
        authorize_if actor_attribute_equals(:admin?, true)
      end

      policy private_action?() do
        authorize_if {CountedCheck, permission: :private_read}
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if {CountedCheck, permission: :domain_mutation}
      end

      policy action_type(:read) do
        authorize_if {CountedCheck, permission: :domain_read}
      end
    end
  end

  test "known action prunes inapplicable checks before runtime simplification" do
    action = Ash.Resource.Info.action(Resource, :read)
    policies = Ash.Policy.Info.policies(Domain, Resource)
    refute policies == Ash.Policy.Info.policies(Resource)

    authorizer = authorizer(action.name, %{domain_read: true, resource_read: true})

    assert {:ok, true, _} = Ash.Policy.Policy.solve(authorizer)
    refute_received {:simplified_permission, :domain_mutation}
    refute_received {:simplified_permission, :resource_mutation}
    assert_received {:simplified_permission, :domain_read}
    assert_received {:simplified_permission, :resource_read}
  end

  test "domain prepend, action types, private actions, and bypasses retain their decisions" do
    permissions = [
      :domain_read,
      :resource_read,
      :domain_mutation,
      :resource_mutation,
      :private_read
    ]

    allowed = Map.new(permissions, &{&1, true})
    actors = [%{}, %{admin?: true}, allowed | Enum.map(permissions, &Map.put(allowed, &1, false))]

    for actor <- actors,
        name <- [:read, :bypass_read, :private_read, :create, :update, :destroy] do
      expected =
        cond do
          name == :bypass_read and actor[:admin?] == true ->
            true

          name == :private_read ->
            actor[:domain_read] == true and actor[:resource_read] == true and
              actor[:private_read] == true

          name in [:read, :bypass_read] ->
            actor[:domain_read] == true and actor[:resource_read] == true

          true ->
            actor[:domain_mutation] == true and actor[:resource_mutation] == true
        end

      assert {:ok, ^expected, _} = Ash.Policy.Policy.solve(authorizer(name, actor))
    end
  end

  test "runtime custom simplification and actor decisions remain fresh" do
    for allowed <- [true, false, true] do
      assert {:ok, ^allowed, _} =
               Ash.Policy.Policy.solve(
                 authorizer(:read, %{domain_read: allowed, resource_read: true})
               )

      assert_received {:simplified_permission, :domain_read}
      assert_received {:simplified_permission, :resource_read}
    end
  end

  test "specialized action facts remain available to policy breakdowns" do
    assert {:ok, true, solved} =
             Ash.Policy.Policy.solve(authorizer(:read, %{domain_read: true, resource_read: true}))

    action_modules = [
      Ash.Policy.Check.Action,
      Ash.Policy.Check.ActionType,
      Ash.Policy.Check.PrivateAction
    ]

    for policy <- solved.policies,
        reference = {check, options} <- List.wrap(policy.condition),
        check in action_modules do
      assert Ash.Policy.Policy.fetch_fact(solved.facts, reference) ==
               {:ok, check.match?(nil, %{action: solved.action, resource: Resource}, options)}
    end
  end

  test "resource-only cached expressions keep their authorization decisions" do
    for allowed <- [true, false] do
      initial = authorizer(:read, %{resource_read: allowed})
      initial = %{initial | domain: nil, policies: Ash.Policy.Info.policies(Resource)}
      assert {:ok, ^allowed, _} = Ash.Policy.Policy.solve(initial)
    end
  end

  test "field cache and noncached field fallback keep their authorization decisions" do
    field_policies =
      Resource
      |> Ash.Policy.Info.field_policy_expressions()
      |> Map.keys()
      |> Enum.find(fn policies -> Enum.any?(policies, &(:secret in &1.fields)) end)

    assert is_list(field_policies)

    for cached? <- [true, false], allowed <- [true, false] do
      policies =
        if cached? do
          field_policies
        else
          Enum.map(field_policies, &%{&1 | description: "runtime field policy"})
        end

      initial = authorizer(:read, %{field_read: allowed})
      assert {:ok, ^allowed, _} = Ash.Policy.Policy.solve(%{initial | policies: policies})
    end
  end

  test "nil-action fallback still simplifies custom checks at runtime" do
    policies = [
      %Ash.Policy.Policy{
        condition: [{Ash.Policy.Check.Static, result: true}],
        policies: [
          %Ash.Policy.Check{
            check: {CountedCheck, permission: :without_action},
            check_module: CountedCheck,
            check_opts: [permission: :without_action],
            type: :authorize_if
          }
        ]
      }
    ]

    for allowed <- [true, false] do
      initial = authorizer(:read, %{without_action: allowed})

      assert {:ok, ^allowed, _} =
               Ash.Policy.Policy.solve(%{initial | action: nil, policies: policies})

      assert_received {:simplified_permission, :without_action}
    end
  end

  test "remaining runtime checks still expand their custom invariants" do
    policies =
      for expected <- [true, false] do
        reference = {RuntimeInvariantCheck, expected: expected}

        %Ash.Policy.Policy{
          condition: [{Ash.Policy.Check.Static, result: true}],
          policies: [
            %Ash.Policy.Check{
              check: reference,
              check_module: RuntimeInvariantCheck,
              check_opts: [expected: expected],
              type: :authorize_if
            }
          ]
        }
      end

    initial = authorizer(:read, %{})

    assert {:error, _, :unsatisfiable} =
             Ash.Policy.Policy.solve(%{initial | policies: policies})

    assert_received :runtime_invariant_checked
  end

  test "nil-policy field authorization retains its existing fallback" do
    initial = authorizer(:read, %{})
    assert {:ok, false, _} = Ash.Policy.Policy.solve(%{initial | policies: nil})
  end

  defp authorizer(name, actor) do
    %Ash.Policy.Authorizer{
      resource: Resource,
      domain: Domain,
      action: Ash.Resource.Info.action(Resource, name),
      actor: actor,
      policies: Ash.Policy.Info.policies(Domain, Resource)
    }
  end
end
