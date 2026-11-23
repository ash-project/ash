# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.StreamTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:destroy, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, offset?: true, required?: false
      end

      read :read_with_no_pagination

      create :create do
        primary? true

        change relate_actor(:author, allow_nil?: true)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true

      timestamps()
    end

    relationships do
      belongs_to :author, Author, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end

      policy action_type(:update) do
        authorize_if relates_to_actor_via(:author)
      end
    end
  end

  defmodule CappedPost do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*, update: :*]

      read :read do
        primary? true
        pagination keyset?: true, offset?: true, required?: false, max_page_size: 2
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end
  end

  defmodule NoLimitOffsetEts do
    @moduledoc false
    @behaviour Ash.DataLayer

    def can?(_resource, :limit), do: false
    def can?(_resource, :offset), do: false
    def can?(resource, feature), do: Ash.DataLayer.Ets.can?(resource, feature)

    for {name, arity} <- Ash.DataLayer.Ets.__info__(:functions),
        name not in [:can?, :spark_dsl_config, :explain] and
          not String.starts_with?(Atom.to_string(name), "__") do
      args = Macro.generate_arguments(arity, __MODULE__)
      defdelegate unquote(name)(unquote_splicing(args)), to: Ash.DataLayer.Ets
    end
  end

  defmodule UnpaginatedPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: NoLimitOffsetEts

    actions do
      defaults [:destroy, create: [:title], update: [:title]]

      read :read do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end
  end

  describe "action that can't be streamed with keyset or offset" do
    setup do
      Ash.bulk_create!([%{title: "a"}, %{title: "b"}], UnpaginatedPost, :create)
      :ok
    end

    test "stream_strategy returns an error tuple when offset is allowed" do
      query = Ash.Query.for_read(UnpaginatedPost, :read)

      assert {:error, %Ash.Error.Invalid.NonStreamableAction{types: [:keyset, :offset]}} =
               Ash.Actions.Read.Stream.stream_strategy(query, nil, :offset)
    end

    test "Ash.stream! raises NonStreamableAction when offset is allowed" do
      assert_raise Ash.Error.Invalid.NonStreamableAction, fn ->
        UnpaginatedPost |> Ash.stream!(allow_stream_with: :offset) |> Enum.to_list()
      end
    end

    test "full_read still streams every record" do
      streamed = UnpaginatedPost |> Ash.stream!(allow_stream_with: :full_read) |> Enum.to_list()

      assert length(streamed) >= 2
      assert Enum.sort_by(streamed, & &1.id) == Enum.sort_by(Ash.read!(UnpaginatedPost), & &1.id)
    end

    test "bulk update and destroy return NoMatchingBulkStrategy when offset is allowed" do
      opts = [strategy: :stream, allow_stream_with: :offset, return_errors?: true]

      assert %Ash.BulkResult{
               status: :error,
               errors: [%Ash.Error.Invalid{errors: [%Ash.Error.Invalid.NoMatchingBulkStrategy{}]}]
             } = Ash.bulk_update(UnpaginatedPost, :update, %{title: "z"}, opts)

      assert %Ash.BulkResult{
               status: :error,
               errors: [%Ash.Error.Invalid{errors: [%Ash.Error.Invalid.NoMatchingBulkStrategy{}]}]
             } = Ash.bulk_destroy(UnpaginatedPost, :destroy, %{}, opts)
    end
  end

  test "a batch_size above the action's max_page_size still streams every record" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(CappedPost, :create)

    count =
      CappedPost
      |> Ash.stream!(batch_size: 1_000)
      |> Enum.count()

    assert count == 10
  end

  test "a non-positive batch_size is rejected rather than looping forever" do
    assert_raise ArgumentError, ~r/must be a positive integer/, fn ->
      Post
      |> Ash.stream!(batch_size: 0)
      |> Enum.to_list()
    end

    assert_raise ArgumentError, ~r/must be a positive integer/, fn ->
      Post
      |> Ash.stream!(batch_size: -5, stream_with: :offset)
      |> Enum.to_list()
    end
  end

  test "records can be streamed" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.stream!(batch_size: 100_000)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed using limit/offset strategy" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.stream!(batch_size: 5, stream_with: :offset)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed using limit/offset strategy with no pagination, count % batch_size != 0" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.Query.for_read(:read_with_no_pagination)
      |> Ash.stream!(batch_size: 4, stream_with: :offset)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed using limit/offset strategy with no pagination, count % batch_size == 0" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.Query.for_read(:read_with_no_pagination)
      |> Ash.stream!(batch_size: 5, stream_with: :offset)
      |> Enum.count()

    assert count == 10
  end

  test "terminates when using limit/offset with no pagination if there are no results" do
    count =
      Post
      |> Ash.Query.for_read(:read_with_no_pagination)
      |> Ash.stream!(batch_size: 5, stream_with: :offset)
      |> Enum.count()

    assert count == 0
  end

  test "records can be streamed using full_read strategy" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.stream!(batch_size: 5, stream_with: :full_read)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed, and the overall limit will be honored" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Ash.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.Query.limit(7)
      |> Ash.stream!(batch_size: 5)
      |> Enum.count()

    assert count == 7
  end

  test "runs successfully when actor is provided without authorize? - Issue #2224" do
    actor =
      Author
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!()

    assert %Ash.BulkResult{
             records: [
               %{title: "updated"},
               %{title: "updated"}
             ]
           } =
             Ash.bulk_create!(
               [%{title: "foo"}, %{title: "bar"}],
               Post,
               :create,
               return_stream?: true,
               return_records?: true,
               actor: actor
             )
             |> Stream.map(fn {:ok, result} ->
               result
             end)
             |> Ash.bulk_update!(
               :update,
               %{title: "updated"},
               resource: Post,
               strategy: :stream,
               return_records?: true,
               actor: actor
             )
  end
end
