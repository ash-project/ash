# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.CalculationContextTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Post.Calculations.QueryContexts do
    use Ash.Resource.Calculation

    def calculate(records, _opts, context) do
      Enum.map(records, fn _ ->
        Map.take(context.source_context, [:preparation_time, :before_transaction])
      end)
    end
  end

  defmodule Post.Preparations.PreparationTime do
    use Ash.Resource.Preparation

    require Ash.Query

    @impl Ash.Resource.Preparation
    def prepare(query, _opts, context) do
      Ash.Query.put_context(query, :preparation_time, "preparation time")
    end
  end

  defmodule Post.Preparations.BeforeTransaction do
    use Ash.Resource.Preparation

    require Ash.Query

    @impl Ash.Resource.Preparation
    def prepare(query, _opts, context) do
      Ash.Query.before_transaction(query, fn query ->
        Ash.Query.put_context(query, :before_transaction, "before transaction")
      end)
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource Post
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      read :context_read do
        prepare __MODULE__.Preparations.PreparationTime
        prepare __MODULE__.Preparations.BeforeTransaction
      end
    end

    attributes do
      uuid_v7_primary_key :id
      attribute :title, :string
    end

    calculations do
      calculate :query_contexts, :map, __MODULE__.Calculations.QueryContexts do
      end
    end
  end

  setup_all do
    Ash.Seed.seed!(Post, %{title: "First Post"})
    :ok
  end

  test "calculations can access context added in the query at preparation time" do
    result =
      Post
      |> Ash.Query.for_read(:context_read, %{}, authorize?: false)
      |> Ash.Query.load([:query_contexts])
      |> Ash.read_one!()

    assert %{preparation_time: "preparation time"} = result.query_contexts
  end

  test "calculations can access context added in before_transaction hooks" do
    result =
      Post
      |> Ash.Query.for_read(:context_read, %{}, authorize?: false)
      |> Ash.Query.load([:query_contexts])
      |> Ash.read_one!()

    assert %{before_transaction: "before transaction"} = result.query_contexts
  end
end
