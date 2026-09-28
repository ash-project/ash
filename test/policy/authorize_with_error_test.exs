# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.AuthorizeWithErrorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :owner_id, :string, public?: true
    end

    policies do
      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type([:read, :update, :destroy]) do
        authorize_if expr(owner_id == ^actor(:id))
      end
    end

    actions do
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination offset?: true, required?: false, countable: true
      end
    end
  end

  setup do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{owner_id: "alice"})
      |> Ash.create!(authorize?: false)

    %{query: Ash.Query.do_filter(Post, id: post.id)}
  end

  test "read returns a policy error", %{query: query} do
    assert {:error, %Ash.Error.Forbidden{errors: [%Ash.Error.Forbidden.Policy{}]}} =
             Ash.read(query, actor: %{id: "bob"}, authorize_with: :error)
  end

  test "paginated read with count returns a policy error", %{query: query} do
    assert {:error, %Ash.Error.Forbidden{errors: [%Ash.Error.Forbidden.Policy{}]}} =
             Ash.read(query,
               actor: %{id: "bob"},
               authorize_with: :error,
               page: [limit: 10, count: true]
             )
  end

  test "atomic bulk destroy returns a policy error", %{query: query} do
    assert %Ash.BulkResult{
             status: :error,
             errors: [%Ash.Error.Forbidden{errors: [%Ash.Error.Forbidden.Policy{}]}]
           } =
             Ash.bulk_destroy(query, :destroy, %{},
               actor: %{id: "bob"},
               authorize_with: :error,
               return_errors?: true
             )
  end

  test "atomic bulk update returns a policy error", %{query: query} do
    assert %Ash.BulkResult{
             status: :error,
             errors: [%Ash.Error.Forbidden{errors: [%Ash.Error.Forbidden.Policy{}]}]
           } =
             Ash.bulk_update(query, :update, %{},
               actor: %{id: "bob"},
               authorize_with: :error,
               return_errors?: true
             )
  end
end
