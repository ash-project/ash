# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.MixedPathTest do
  @moduledoc """
  Phase 4 — mixed `exists/2` paths that traverse relationships *and*
  embedded-array attributes in the same expression.

  Covers in-memory (ETS) behavior. AshPostgres equivalents live in
  `ash_postgres/test/embedded_array_exists_test.exs`.

  See `specs/embedded-array-exists.md`.
  """
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.EmbeddedArray.Company
  alias Ash.Test.EmbeddedArray.Estimate

  setup do
    cheap_company = create_company("CheapCo")
    pricey_company = create_company("PriceyCo")

    create_estimate(%{
      title: "cheap",
      company_id: cheap_company.id,
      options: [%{name: "basic", total_amt: Decimal.new("50")}]
    })

    create_estimate(%{
      title: "expensive",
      company_id: pricey_company.id,
      options: [%{name: "premium", total_amt: Decimal.new("150")}]
    })

    %{cheap_company: cheap_company, pricey_company: pricey_company}
  end

  describe "exists/2 over mixed (relationship → embedded array) paths" do
    test "exists(estimates.options, total_amt > 100) on Company", %{
      cheap_company: cheap,
      pricey_company: pricey
    } do
      results =
        Company
        |> Ash.Query.filter(exists(estimates.options, total_amt > 100))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert pricey.id in results
      refute cheap.id in results
    end

    test "string equality through mixed path", %{cheap_company: cheap, pricey_company: pricey} do
      results =
        Company
        |> Ash.Query.filter(exists(estimates.options, name == "basic"))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert cheap.id in results
      refute pricey.id in results
    end

    test "parent(...) reaches the calling Company scope", %{pricey_company: pricey} do
      results =
        Company
        |> Ash.Query.filter(
          exists(estimates.options, total_amt > 100 and parent(name) == "PriceyCo")
        )
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert pricey.id in results
    end
  end

  defp create_company(name) do
    Company
    |> Ash.Changeset.for_create(:create, %{name: name})
    |> Ash.create!()
  end

  defp create_estimate(attrs) do
    Estimate
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end
end
