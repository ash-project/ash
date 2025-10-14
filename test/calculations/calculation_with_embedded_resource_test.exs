# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.CalculationWithEmbeddedResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset

  defmodule Profile do
    use Ash.Resource, data_layer: :embedded

    attributes do
      uuid_primary_key :id do
        generated? false
        public? true
        writable? true
      end

      attribute :platform, :string do
        public?(true)
      end
    end
  end

  defmodule Calculation.Profile do
    use Ash.Resource.Calculation

    def calculate(records, _opts, _context) do
      records |> Enum.map(&get_profile/1)
    end

    def get_profile(author) do
      Changeset.for_create(
        Profile,
        :create,
        %{id: author.id, platform: "Some platform"}
      )
      |> Ash.create!()
      |> List.wrap()
    end
  end

  defmodule Author do
    use Ash.Resource,
      domain: Ash.Test.Calculations.CalculationWithEmbeddedResourceTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id, writable?: true
    end

    calculations do
      calculate :profiles,
                {:array, Profile},
                Calculation.Profile do
        public? true
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Author
    end
  end

  test "embedded resources can be returned by calculations" do
    assert %Author{profiles: profiles} =
             Changeset.for_create(Author, :create, %{}) |> Ash.create!() |> Ash.load!(:profiles)

    assert [%{platform: "Some platform"}] = profiles
  end
end
