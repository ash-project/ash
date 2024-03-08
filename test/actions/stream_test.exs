defmodule Ash.Test.Actions.StreamTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        pagination keyset?: true, offset?: true, required?: false
      end

      read :read_with_no_pagination
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true

      timestamps()
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
end
