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

  defmodule AdminDoc do
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

      attribute :secret_score, :integer do
        public? true
      end
    end

    actions do
      default_accept [:title, :secret_score]
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
        authorize_if actor_attribute_equals(:admin, true)
      end

      field_policy :* do
        authorize_if always()
      end
    end
  end

  test "plain sort on a field the actor cannot read raises instead of silently breaking pagination" do
    for i <- 1..4 do
      AdminDoc
      |> Ash.Changeset.for_create(:create, %{title: "t#{i}", secret_score: i * 10},
        authorize?: false
      )
      |> Ash.create!()
    end

    # a field policy forbids a non-admin from reading `secret_score`, so the
    # cursor could only be built from the redaction marker. Rather than silently
    # returning an empty second page, the read is rejected.
    assert {:error, %Ash.Error.Invalid{errors: errors}} =
             AdminDoc
             |> Ash.Query.sort(secret_score: :asc)
             |> Ash.read(actor: %{admin: false}, page: [limit: 2])

    assert Enum.any?(errors, &match?(%Ash.Error.Page.KeysetSortOnForbiddenField{}, &1))

    # an admin can read the field, so pagination works
    page1 =
      AdminDoc
      |> Ash.Query.sort(secret_score: :asc)
      |> Ash.read!(actor: %{admin: true}, page: [limit: 2])

    cursor = List.last(page1.results).__metadata__.keyset

    page2 =
      AdminDoc
      |> Ash.Query.sort(secret_score: :asc)
      |> Ash.read!(actor: %{admin: true}, page: [limit: 2, after: cursor])

    assert length(page1.results) == 2
    assert length(page2.results) == 2
  end

  test "plain sort on a readable field still paginates under field policies" do
    for {owner, score} <- [{"me", 10}, {"someone_else", 20}, {"someone_else", 30}] do
      OwnedDoc
      |> Ash.Changeset.for_create(
        :create,
        %{title: "doc-#{score}", owner_id: owner, secret_score: score},
        authorize?: false
      )
      |> Ash.create!()
    end

    page1 =
      OwnedDoc
      |> Ash.Query.sort(title: :asc)
      |> Ash.read!(actor: %{id: "me"}, page: [limit: 2])

    cursor = List.last(page1.results).__metadata__.keyset

    page2 =
      OwnedDoc
      |> Ash.Query.sort(title: :asc)
      |> Ash.read!(actor: %{id: "me"}, page: [limit: 2, after: cursor])

    assert length(page1.results) == 2
    assert length(page2.results) == 1
  end

  test "plain sort on a record-dependent forbidden field raises" do
    for {owner, score} <- [{"me", 10}, {"someone_else", 20}, {"someone_else", 30}] do
      OwnedDoc
      |> Ash.Changeset.for_create(
        :create,
        %{title: "doc", owner_id: owner, secret_score: score},
        authorize?: false
      )
      |> Ash.create!()
    end

    # `secret_score` is readable for the actor's own rows but not others'. The
    # field is loaded to sort, so the cursor would otherwise be built from the
    # real value for rows the actor can't read. Checking the redacted result
    # rejects the read rather than leaking those values through the cursor.
    assert {:error, %Ash.Error.Invalid{errors: errors}} =
             OwnedDoc
             |> Ash.Query.sort(secret_score: :asc)
             |> Ash.read(actor: %{id: "me"}, page: [limit: 2])

    assert Enum.any?(errors, &match?(%Ash.Error.Page.KeysetSortOnForbiddenField{}, &1))
  end
end
