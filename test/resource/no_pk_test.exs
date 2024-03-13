defmodule Ash.Test.Resource.NoPkTest do
  @moduledoc false
  use ExUnit.Case, async: true
  alias Ash.Changeset
  alias Ash.Test.Domain, as: Domain

  defmodule Temperature do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    resource do
      require_primary_key? false
    end

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      attribute :time, :utc_datetime_usec do
        public?(true)
      end

      attribute :temperature, :float do
        public?(true)
      end
    end

    relationships do
      belongs_to :location, Ash.Test.Resource.NoPkTest.Location do
        public?(true)
      end
    end
  end

  defmodule Location do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :location, :string do
        public?(true)
      end
    end
  end

  test "records can be written and read" do
    assert {:ok, expected} =
             Temperature
             |> Changeset.for_action(:create, %{
               temperature: :rand.uniform()
             })
             |> Ash.create()

    assert {:ok, [actual]} = Ash.read(Temperature)
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
             |> Ash.create()

    assert {:ok, actual} =
             Temperature
             |> Changeset.for_action(:create, %{
               temperature: :rand.uniform(),
               location_id: location.id
             })
             |> Ash.create()

    assert actual.location_id == location.id
  end
end
