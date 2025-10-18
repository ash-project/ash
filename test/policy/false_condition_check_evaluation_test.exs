# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FalseConditionCheckEvaluationTest do
  @moduledoc false
  use ExUnit.Case

  defmodule ChangesetOnlyCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    def describe(_), do: "changeset only check"

    # Only handles changeset context, will crash on query context.
    def match?(_actor, %{changeset: %Ash.Changeset{}} = _context, _opts) do
      true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.FalseConditionCheckEvaluationTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:title, :string, allow_nil?: false, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:read, :destroy, create: :*, update: :*])
    end

    policies do
      policy action_type([:create, :update]) do
        authorize_if(ChangesetOnlyCheck)
      end

      policy always() do
        authorize_if(always())
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource(Post)
    end
  end

  describe "policy check evaluation when condition is false" do
    test "does not evaluate check when condition is false" do
      # This should not crash even though ChangesetOnlyCheck only handles changesets.
      # The policy condition (action_type [:create, :update]) is false for read actions,
      # so ChangesetOnlyCheck.match?/3 should not be called.
      assert {:ok, []} = Ash.read(Post)
    end

    test "evaluates check when condition is true" do
      # This should work because ChangesetOnlyCheck handles changeset context.
      assert {:ok, _post} = Ash.create(Post, %{title: "test"})
    end
  end
end
