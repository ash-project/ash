defmodule Ash.Test.Actions.StreamTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        pagination keyset?: true, offset?: true, required?: false
      end

      read :read_with_no_pagination
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false

      timestamps()
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Post
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "records can be streamed" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Api.bulk_create!(Post, :create)

    count =
      Post
      |> Api.stream!(batch_size: 100_000)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed using limit/offset strategy" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Api.bulk_create!(Post, :create)

    count =
      Post
      |> Api.stream!(batch_size: 5, stream_with: :offset)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed using full_read strategy" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Api.bulk_create!(Post, :create)

    count =
      Post
      |> Api.stream!(batch_size: 5, stream_with: :full_read)
      |> Enum.count()

    assert count == 10
  end

  test "records can be streamed, and the overall limit will be honored" do
    1..10
    |> Stream.map(&%{title: "title#{&1}"})
    |> Api.bulk_create!(Post, :create)

    count =
      Post
      |> Ash.Query.limit(7)
      |> Api.stream!(batch_size: 5)
      |> Enum.count()

    assert count == 7
  end
end
