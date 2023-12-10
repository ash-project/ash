defmodule Ash.Test.Resource.NoPkTest do
  @moduledoc false
  use ExUnit.Case, async: true
  alias Ash.Changeset

  defmodule Temperature do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    resource do
      require_primary_key? false
    end

    ets do
      private? true
    end

    actions do
      defaults [:create, :read]
    end

    attributes do
      attribute :time, :utc_datetime_usec
      attribute :temperature, :float
    end

    relationships do
      belongs_to :location, Ash.Test.Resource.NoPkTest.Location do
        attribute_writable? true
      end
    end
  end

  defmodule Location do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :location, :string
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource Temperature
      resource Location
    end
  end

  test "records can be written and read" do
    assert {:ok, expected} =
             Temperature
             |> Changeset.for_action(:create, %{
               temperature: :rand.uniform()
             })
             |> Api.create()

    assert {:ok, [actual]} = Api.read(Temperature)
    assert expected.time == actual.time
    assert expected.temperature == actual.temperature
  end

  test "records can belong to other resources" do
    assert {:ok, location} =
             Location
             |> Changeset.for_action(:create, %{
               location:
                 "Taumata­whakatangihanga­koauau­o­tamatea­turi­pukaka­piki­maunga­horo­nuku­pokai­whenua­ki­tana­tahu"
             })
             |> Api.create()

    assert {:ok, actual} =
             Temperature
             |> Changeset.for_action(:create, %{
               temperature: :rand.uniform(),
               location_id: location.id
             })
             |> Api.create()

    assert actual.location_id == location.id
  end
end
