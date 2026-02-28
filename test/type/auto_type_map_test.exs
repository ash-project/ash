defmodule Ash.Test.Type.AutoTypeMapTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule MapResource do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :score, :integer, public?: true
    end

    calculations do
      # Map literal with atom keys and typed values
      calculate :summary, :auto, expr(%{name: name, score: score}) do
        public?(true)
      end
    end
  end

  describe "auto type resolution for map literals" do
    test "resolves map literal to Ash.Type.Map with field constraints" do
      calc = Ash.Resource.Info.calculation(MapResource, :summary)
      assert calc.type == Ash.Type.Map

      # Check that field constraints are set
      fields = calc.constraints[:fields]
      assert is_list(fields)

      name_field = Keyword.get(fields, :name)
      assert name_field[:type] == Ash.Type.String

      score_field = Keyword.get(fields, :score)
      assert score_field[:type] == Ash.Type.Integer
    end

    test "map literal calculation returns correct values at runtime" do
      record = Ash.Seed.seed!(MapResource, %{name: "Alice", score: 95})
      loaded = Ash.load!(record, :summary)
      assert loaded.summary == %{name: "Alice", score: 95}
    end
  end
end
