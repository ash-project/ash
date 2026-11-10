# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.DomainPolicyCompositionTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Crux.Expression, only: [b: 1, is_variable: 1]

  alias Ash.Policy.Check
  alias Ash.Policy.Policy

  defmodule CountedCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    def describe(opts), do: "permission #{inspect(opts[:permission])}"

    def simplify({_, opts} = ref, _context) do
      send(self(), {:simplified, opts[:permission]})
      ref
    end

    def match?(actor, _context, opts) do
      send(self(), {:evaluated, opts[:permission]})
      Map.get(actor || %{}, opts[:permission], false)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.DomainPolicyCompositionTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
      read :special
      read :internal, public?: false
    end

    policies do
      policy action_type(:read) do
        authorize_if {CountedCheck, permission: :resource_read}
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if {CountedCheck, permission: :resource_mutation}
      end

      policy action(:special) do
        forbid_unless {CountedCheck, permission: :resource_special}
        authorize_if always()
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, extensions: [Ash.Policy.Authorizer], validate_config_inclusion?: false

    resources do
      resource Post
    end

    policies do
      bypass actor_attribute_equals(:admin?, true) do
        authorize_if always()
      end

      bypass action(:special) do
        authorize_if {CountedCheck, permission: :special_bypass}
      end

      policy private_action?() do
        authorize_if {CountedCheck, permission: :domain_private}
      end

      policy action_type(:read) do
        authorize_if {CountedCheck, permission: :domain_read}
      end

      policy action_type([:create, :update, :destroy]) do
        authorize_if {CountedCheck, permission: :domain_mutation}
      end
    end
  end

  @permissions [
    :resource_read,
    :resource_mutation,
    :resource_special,
    :special_bypass,
    :domain_private,
    :domain_read,
    :domain_mutation
  ]

  @actions [:read, :special, :internal, :create, :update, :destroy]

  describe "with domain policies" do
    test "the domain persists a template of its policies" do
      assert %{policies: policies, template: template} =
               Ash.Policy.Info.domain_policy_expression(Domain)

      assert policies == Ash.Policy.Info.policies(Domain)
      refute is_boolean(template)
    end

    test "decisions match building the expression from scratch" do
      everything = Map.new(@permissions, &{&1, true})

      actors =
        [nil, %{}, %{admin?: true}, everything] ++
          Enum.map(@permissions, &Map.put(everything, &1, false)) ++
          Enum.map(@permissions, &%{&1 => true})

      for actor <- actors, action <- @actions do
        composed = authorizer(action, actor)

        # Changing a description misses every cache, so this builds the full
        # expression at runtime (see `Ash.Test.Policy.KnownActionFallbackTest`).
        uncached = %{
          composed
          | policies: Enum.map(composed.policies, &%{&1 | description: "uncached"})
        }

        assert {:ok, expected, _} = Policy.solve(uncached)

        assert {:ok, ^expected, _} = Policy.solve(composed),
               "expected #{expected} for #{inspect(action)} with #{inspect(actor)}"
      end
    end

    test "checks in policies for other actions are neither simplified nor evaluated" do
      assert {:ok, true, _} =
               Policy.solve(authorizer(:read, %{domain_read: true, resource_read: true}))

      assert_received {:evaluated, :domain_read}
      assert_received {:evaluated, :resource_read}

      for permission <- @permissions -- [:domain_read, :resource_read] do
        refute_received {:simplified, ^permission}
        refute_received {:evaluated, ^permission}
      end
    end

    test "actions are authorized end to end" do
      allowed = %{domain_read: true, resource_read: true}

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo"}, actor: %{admin?: true})
        |> Ash.create!()

      assert [%{id: id}] = Ash.read!(Post, actor: allowed)
      assert id == post.id
      assert [] = Ash.read!(Post, actor: %{resource_read: true})
      assert [_] = Ash.read!(Post, actor: %{admin?: true})

      assert {:error, %Ash.Error.Forbidden{}} =
               Post
               |> Ash.Changeset.for_create(:create, %{title: "foo"}, actor: allowed)
               |> Ash.create()

      assert {:ok, _} =
               Post
               |> Ash.Changeset.for_create(:create, %{title: "foo"},
                 actor: %{domain_mutation: true, resource_mutation: true}
               )
               |> Ash.create()
    end
  end

  property "the domain template composed with the resource fold is equivalent to the full expression" do
    check all(
            domain_policies <- list_of(policy(), max_length: 4),
            resource_policies <- list_of(policy(), max_length: 4),
            action_name <- member_of(@actions),
            max_runs: 500
          ) do
      action = Ash.Resource.Info.action(Post, action_name)

      expected = Policy.static_expression(domain_policies ++ resource_policies, Post, action)

      composed =
        domain_policies
        |> Policy.domain_template(Domain)
        |> Policy.compose_domain_template(
          Policy.static_fold(resource_policies, Post, action),
          Post,
          action
        )

      refute Enum.any?(variables(composed), &match?({Policy, _}, &1))
      assert equivalent?(expected, composed)
    end
  end

  defp authorizer(action, actor) do
    %Ash.Policy.Authorizer{
      resource: Post,
      domain: Domain,
      action: Ash.Resource.Info.action(Post, action),
      actor: actor,
      policies: Ash.Policy.Info.policies(Domain, Post)
    }
  end

  defp free(n), do: {Check.ActorAttributeEquals, attribute: :"flag_#{n}", value: true}

  defp condition do
    member_of([
      {Check.ActionType, type: [:read]},
      {Check.ActionType, type: [:create, :update]},
      {Check.Action, action: [:special]},
      {Check.Action, action: [:read, :destroy]},
      {Check.PrivateAction, []},
      {Check.Static, result: true},
      {Check.Static, result: false},
      {Check.ActorAbsent, []},
      free(1),
      free(2)
    ])
  end

  defp check_ref do
    member_of([
      free(1),
      free(2),
      free(3),
      {Check.Static, result: true},
      {Check.Static, result: false},
      {Check.ActionType, type: [:read]},
      {Check.ActorAbsent, []}
    ])
  end

  defp policy do
    gen all(
          condition <- list_of(condition(), max_length: 2),
          checks <-
            list_of(
              tuple(
                {member_of([:authorize_if, :forbid_if, :authorize_unless, :forbid_unless]),
                 check_ref()}
              ),
              min_length: 1,
              max_length: 3
            ),
          bypass? <- boolean()
        ) do
      %Policy{
        condition: condition,
        bypass?: bypass?,
        policies:
          Enum.map(checks, fn {type, {module, opts} = ref} ->
            %Check{type: type, check: ref, check_module: module, check_opts: opts}
          end)
      }
    end
  end

  defp equivalent?(left, right) do
    variables = Enum.uniq(variables(left) ++ variables(right))

    variables
    |> assignments()
    |> Enum.all?(&(eval(left, &1) == eval(right, &1)))
  end

  defp variables(expression) do
    {_, variables} =
      Crux.Expression.postwalk(expression, [], fn
        variable, acc when is_variable(variable) -> {variable, [variable | acc]}
        other, acc -> {other, acc}
      end)

    variables
  end

  defp assignments([]), do: [%{}]

  defp assignments([variable | rest]) do
    for assignment <- assignments(rest), value <- [true, false] do
      Map.put(assignment, variable, value)
    end
  end

  defp eval(value, _assignment) when is_boolean(value), do: value
  defp eval(b(left and right), assignment), do: eval(left, assignment) and eval(right, assignment)
  defp eval(b(left or right), assignment), do: eval(left, assignment) or eval(right, assignment)
  defp eval(b(not value), assignment), do: not eval(value, assignment)
  defp eval(variable, assignment) when is_variable(variable), do: Map.fetch!(assignment, variable)
end
