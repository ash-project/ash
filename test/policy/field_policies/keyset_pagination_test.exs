# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FieldPolicy.KeysetPaginationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule OwnedDoc do
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

      attribute :title, :string do
        public? true
      end

      attribute :owner_id, :string do
        public? true
      end

      attribute :secret_score, :integer do
        public? true
      end
    end

    actions do
      default_accept [:title, :owner_id, :secret_score]
      defaults [:create]

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :secret_score do
        authorize_if expr(owner_id == ^actor(:id))
      end

      field_policy :* do
        authorize_if always()
      end
    end
  end

  test "sort_input over a record-dependent field policy encodes nil in cursors for hidden records" do
    for {owner, score} <- [{"me", 10}, {"someone_else", 20}, {"someone_else", 30}] do
      OwnedDoc
      |> Ash.Changeset.for_create(
        :create,
        %{title: "doc", owner_id: owner, secret_score: score},
        authorize?: false
      )
      |> Ash.create!()
    end

    page =
      OwnedDoc
      |> Ash.Query.sort_input(secret_score: :asc)
      |> Ash.read!(actor: %{id: "me"}, page: [limit: 3])

    keyset_values =
      Enum.map(page.results, fn record ->
        record.__metadata__.keyset
        |> Base.decode64!()
        |> Ash.Helpers.non_executable_binary_to_term([:safe])
        |> List.first()
      end)

    # the actor's own record encodes its real value; records whose
    # secret_score the actor cannot read encode nil, not the value
    assert 10 in keyset_values
    refute 20 in keyset_values
    refute 30 in keyset_values
    assert Enum.count(keyset_values, &is_nil/1) == 2

    # pagination resumes correctly page to page
    page1 =
      OwnedDoc
      |> Ash.Query.sort_input(secret_score: :asc)
      |> Ash.read!(actor: %{id: "me"}, page: [limit: 2])

    cursor = List.last(page1.results).__metadata__.keyset

    page2 =
      OwnedDoc
      |> Ash.Query.sort_input(secret_score: :asc)
      |> Ash.read!(actor: %{id: "me"}, page: [limit: 2, after: cursor])

    all_ids = Enum.map(page1.results ++ page2.results, & &1.id)
    assert length(page1.results) == 2
    assert length(page2.results) == 1
    assert Enum.uniq(all_ids) == all_ids
  end
end
