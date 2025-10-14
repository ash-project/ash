# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
