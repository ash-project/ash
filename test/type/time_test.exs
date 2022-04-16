defmodule Ash.Test.Type.TimeTest do
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

      attribute :time_a, :time
      attribute :time_b, :time, allow_nil?: false
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
        time_a: ~T[08:30:00],
        time_b: ~T[15:45:30]
      })
      |> Api.create!()

    assert post.time_a == ~T[08:30:00]
    assert post.time_b == ~T[15:45:30]
  end
end
