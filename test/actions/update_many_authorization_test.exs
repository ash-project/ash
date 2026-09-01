# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.UpdateManyAuthorizationTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Expr

  defmodule RecordingDataLayer do
    @moduledoc false
    @behaviour Ash.DataLayer
    use Spark.Dsl.Extension, transformers: [], sections: []

    @impl true
    def can?(_, feature)
        when feature in [
               :update_many,
               :update,
               :filter,
               :changeset_filter,
               :boolean_filter,
               :nested_expressions,
               :composite_primary_key
             ],
        do: true

    def can?(_, {:atomic, :update}), do: true
    def can?(_, {:filter_expr, _}), do: true
    def can?(_, _), do: false

    @impl true
    def resource_to_query(resource, domain), do: %{resource: resource, domain: domain}

    @impl true
    def update_many(_resource, changesets, opts) do
      changesets = Enum.to_list(changesets)
      send(self(), {:update_many, changesets, opts})

      # Honor each changeset's filter against its own row, the way a real atomic update would only
      # touch matching rows. A row whose (authorization) filter excludes it is simply not returned.
      updated =
        changesets
        |> Enum.filter(fn cs ->
          {:ok, matched} = Ash.Filter.Runtime.filter_matches(cs.domain, [cs.data], cs.filter)
          matched != []
        end)
        |> Enum.map(fn cs -> elem(Ash.Changeset.apply_attributes(cs), 1) end)

      {:ok, updated}
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered? true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: RecordingDataLayer,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:read, update: [:body]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner_id, :string, allow_nil?: false, public?: true
      attribute :body, :string, public?: true
    end

    policies do
      policy action_type(:update) do
        authorize_if expr(owner_id == ^actor(:id))
      end
    end
  end

  defmodule ArgPost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: RecordingDataLayer,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:read]

      update :update do
        accept [:body]
        argument :as_owner, :string, allow_nil?: false
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner_id, :string, allow_nil?: false, public?: true
      attribute :body, :string, public?: true
    end

    policies do
      policy action_type(:update) do
        authorize_if expr(owner_id == ^arg(:as_owner))
      end
    end
  end

  test "an atomic update_many merges the policy filter into each changeset's filter" do
    record = struct(Post, id: 1, owner_id: "me", body: "before")

    result =
      Ash.update_many([{record, %{body: "after"}}], Post, :update,
        actor: %{id: "me"},
        authorize?: true,
        strategy: [:atomic],
        return_records?: true,
        return_errors?: true
      )

    assert result.status == :success

    assert_receive {:update_many, [changeset], _opts}

    refute is_nil(changeset.filter)
    assert inspect(changeset.filter) =~ "owner_id"
  end

  test "authorizes each changeset individually when rows share atomics but differ in arguments" do
    record1 = struct(ArgPost, id: 1, owner_id: "alice", body: "before")
    record2 = struct(ArgPost, id: 2, owner_id: "bob", body: "before")

    result =
      Ash.update_many(
        [
          {record1, %{body: "after", as_owner: "alice"}},
          {record2, %{body: "after", as_owner: "bob"}}
        ],
        ArgPost,
        :update,
        authorize?: true,
        strategy: [:atomic],
        return_records?: true,
        return_errors?: true
      )

    assert result.status == :success

    # The two rows share identical atomics (both set body "after") but were authorized with
    # different arguments, so each changeset must carry its own policy filter. Grouping the
    # authorization by atomics would apply one row's filter to the other.
    filters =
      collect_update_many_filters([])
      |> Enum.map(&inspect/1)

    assert Enum.count(filters, &(&1 =~ "\"alice\"")) == 1
    assert Enum.count(filters, &(&1 =~ "\"bob\"")) == 1
  end

  test "a forbidden row is not updated even when a sibling row in the batch shares its atomics" do
    # Both rows set body "after" (identical atomics). Row 1 is authorized (its owner matches its
    # `as_owner` arg); row 2 is NOT (owner "alice" but `as_owner` "bob"), so its policy filter
    # `owner_id == "bob"` excludes it. Grouping authorization by atomics would apply row 1's
    # `owner_id == "alice"` filter to row 2 — which its data *does* match — updating it anyway.
    authorized = struct(ArgPost, id: 1, owner_id: "alice", body: "before")
    forbidden = struct(ArgPost, id: 2, owner_id: "alice", body: "before")

    result =
      Ash.update_many(
        [
          {authorized, %{body: "after", as_owner: "alice"}},
          {forbidden, %{body: "after", as_owner: "bob"}}
        ],
        ArgPost,
        :update,
        authorize?: true,
        strategy: [:atomic],
        return_records?: true,
        return_errors?: true
      )

    updated_ids = Enum.map(result.records || [], & &1.id)

    assert 1 in updated_ids
    refute 2 in updated_ids
    assert Enum.all?(result.records || [], &(&1.body == "after"))
  end

  defp collect_update_many_filters(acc) do
    receive do
      {:update_many, changesets, _opts} ->
        collect_update_many_filters(acc ++ Enum.map(changesets, & &1.filter))
    after
      0 -> acc
    end
  end
end
