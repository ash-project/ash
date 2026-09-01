# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.RuntimeCheckRecordFilteringTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule AllowedOnlyCheck do
    @moduledoc false
    use Ash.Policy.Check

    def describe(_), do: "record is allowed"

    def strict_check(_actor, _context, _opts), do: {:ok, :unknown}

    def check(_actor, records, _context, _opts) do
      Enum.filter(records, & &1.allowed)
    end
  end

  defmodule Record do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.RuntimeCheckRecordFilteringTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:allowed, :boolean, allow_nil?: false, public?: true)
    end

    actions do
      default_accept(:*)
      defaults([:read, create: :*])
    end

    policies do
      policy action_type(:create) do
        authorize_if(always())
      end

      policy action_type(:read) do
        access_type(:runtime)
        authorize_if(AllowedOnlyCheck)
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource(Record)
    end
  end

  test "a runtime read check filters every denied record, not just the first" do
    allowed =
      Record |> Ash.Changeset.for_create(:create, %{allowed: true}) |> Ash.create!()

    for _ <- 1..3 do
      Record |> Ash.Changeset.for_create(:create, %{allowed: false}) |> Ash.create!()
    end

    assert {:ok, records} = Ash.read(Record, authorize?: true)
    assert Enum.map(records, & &1.id) == [allowed.id]
  end
end
