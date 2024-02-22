defmodule Ash.Test.Type.EnumTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule Status do
    use Ash.Type.Enum, values: [:open, :Closed, :NeverHappened, :Always_Was]
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :status, Status
    end
  end

  test "it handles exact matches" do
    Post
    |> new(%{status: :open})
    |> Api.create!()
  end

  test "it handles string matches" do
    Post
    |> new(%{status: "open"})
    |> Api.create!()
  end

  test "it handles mixed case string matches" do
    Post
    |> new(%{status: "OpEn"})
    |> Api.create!()
  end

  test "it handles mixed case string matches against mixed case atoms" do
    Post
    |> new(%{status: "nEveRHAppened"})
    |> Api.create!()
  end

  test "it fails on mismatches" do
    assert_raise Ash.Error.Invalid, fn ->
      Post
      |> new(%{status: "what"})
      |> Api.create!()
    end
  end

  test "the values are returned in the introspection function" do
    assert Status.values() == [:open, :Closed, :NeverHappened, :Always_Was]
    assert Status.match("OPEN") == {:ok, :open}
    assert Status.match?(:always_was)
  end
end
