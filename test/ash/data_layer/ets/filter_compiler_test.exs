defmodule Ash.DataLayer.Ets.FilterCompilerTest do
  @moduledoc false
  use ExUnit.Case, async: true
  import Ash.DataLayer.Ets.FilterCompiler, only: [compile: 1]
  require Ash.Query

  defmacro assert_filter_matches(filter, attributes) do
    quote do
      assert {:ok, _} =
               :ets.test_ms(
                 {:pk, unquote(attributes)},
                 [{[{:"$1", :"$2"}], [unquote(filter)], [:"$2"]}]
               )
    end
  end

  defmacro refute_filter_matches(filter, attributes) do
    quote do
      assert {:ok, false} =
               :ets.test_ms(
                 {:pk, unquote(attributes)},
                 [{[{:"$1", :"$2"}], [unquote(filter)], [:"$2"]}]
               )
    end
  end

  defmodule FauxResource do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end
  end

  describe "equality" do
    test "positive match" do
      filter = Ash.Query.filter(FauxResource, name: "Zardoz")
      assert {:ok, spec} = compile(filter)
      assert_filter_matches(spec, %{name: "Zardoz"})
    end

    test "negative match" do
      filter = Ash.Query.filter(FauxResource, name: "Ramirez")
      assert {:ok, spec} = compile(filter)
      refute_filter_matches(spec, %{name: "Zardoz"})
    end
  end

  describe "inequality" do
    test "positive match" do
      filter = Ash.Query.filter(FauxResource, name != "Zardoz")
      assert {:ok, spec} = compile(filter)
      assert_filter_matches(spec, %{name: "Ramirez"})
    end

    test "negative match" do
      filter = Ash.Query.filter(FauxResource, name != "Zardoz")
      assert {:ok, spec} = compile(filter)
      refute_filter_matches(spec, %{name: "Zardoz"})
    end
  end
end
