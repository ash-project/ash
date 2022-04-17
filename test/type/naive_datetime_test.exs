defmodule Ash.Test.Type.NaiveDateTimeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :naive_datetime_a, :naive_datetime
      attribute :naive_datetime_b, :naive_datetime, allow_nil?: false
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

  test "it handles non-empty values" do
    post =
      Post
      |> new(%{
        naive_datetime_a: ~N[2022-04-17 08:30:00],
        naive_datetime_b: ~N[2022-04-17 15:45:30]
      })
      |> Api.create!()

    assert post.naive_datetime_a == ~N[2022-04-17 08:30:00]
    assert post.naive_datetime_b == ~N[2022-04-17 15:45:30]
  end
end
