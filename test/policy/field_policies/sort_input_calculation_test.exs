# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FieldPolicy.SortInputCalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Resource do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
      end

      attribute :points, :integer do
        public? true
      end
    end

    calculations do
      calculate :points_calc, :integer, expr(points * 2) do
        public? true
      end

      calculate :secret_calc, :integer, expr(points * 3) do
        public? true
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :name do
        authorize_if always()
      end

      field_policy :secret_calc do
        authorize_if actor_attribute_equals(:admin, true)
      end

      field_policy :* do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end
  end

  setup do
    for points <- [3, 1, 2] do
      Resource
      |> Ash.Changeset.for_create(:create, %{points: points, name: "name #{points}"})
      |> Ash.create!(authorize?: false)
    end

    :ok
  end

  test "sorting on a calculation works when field policies are present" do
    assert [1, 2, 3] =
             Resource
             |> Ash.Query.sort(points_calc: :asc)
             |> Ash.read!(actor: %{}, authorize?: true)
             |> Enum.map(& &1.points)

    assert [3, 2, 1] =
             Resource
             |> Ash.Query.sort(points_calc: :desc)
             |> Ash.read!(actor: %{}, authorize?: true)
             |> Enum.map(& &1.points)
  end

  test "sorting on a calculation via sort_input works when field policies are present" do
    assert [1, 2, 3] =
             Resource
             |> Ash.Query.sort_input(points_calc: :asc)
             |> Ash.read!(actor: %{}, authorize?: true)
             |> Enum.map(& &1.points)

    assert [3, 2, 1] =
             Resource
             |> Ash.Query.sort_input(points_calc: :desc)
             |> Ash.read!(actor: %{}, authorize?: true)
             |> Enum.map(& &1.points)
  end

  test "sort_input on a calculation the actor cannot see replaces the sort key with nil" do
    asc =
      Resource
      |> Ash.Query.sort_input(secret_calc: :asc)
      |> Ash.read!(actor: %{admin: false}, authorize?: true)
      |> Enum.map(& &1.points)

    desc =
      Resource
      |> Ash.Query.sort_input(secret_calc: :desc)
      |> Ash.read!(actor: %{admin: false}, authorize?: true)
      |> Enum.map(& &1.points)

    # every sort key is nil for the unauthorized actor, so direction has no
    # effect, but all rows are still returned and no error is raised
    assert asc == desc
    assert Enum.sort(asc) == [1, 2, 3]

    assert [1, 2, 3] =
             Resource
             |> Ash.Query.sort_input(secret_calc: :asc)
             |> Ash.read!(actor: %{admin: true}, authorize?: true)
             |> Enum.map(& &1.points)

    assert [3, 2, 1] =
             Resource
             |> Ash.Query.sort_input(secret_calc: :desc)
             |> Ash.read!(actor: %{admin: true}, authorize?: true)
             |> Enum.map(& &1.points)
  end
end
