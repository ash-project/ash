defmodule Mix.Tasks.Ash.Gen.ResourceTest do
  use ExUnit.Case, async: true

  alias Mix.Tasks.Ash.Gen.Resource

  test "parse_attributes" do
    assert Resource.parse_attributes([]) == []

    assert Resource.parse_attributes(["name", "age", "integer"]) == [
             {"name", :string},
             {"age", :integer}
           ]

    assert Resource.parse_attributes(["name", "age", "int"]) == [
             {"name", :string},
             {"age", :string},
             {"int", :string}
           ]

    assert Resource.parse_attributes(["a", "atom"]) == [{"a", :atom}]
    assert Resource.parse_attributes(["b", "binary"]) == [{"b", :binary}]
    assert Resource.parse_attributes(["bi", "binary"]) == [{"bi", :binary}]
    assert Resource.parse_attributes(["ci", "cistring"]) == [{"ci", :cistring}]
    assert Resource.parse_attributes(["d", "date"]) == [{"d", :date}]
    assert Resource.parse_attributes(["de", "decimal"]) == [{"de", :decimal}]
    assert Resource.parse_attributes(["float", "float"]) == [{"float", :float}]
    assert Resource.parse_attributes(["func", "function"]) == [{"func", :function}]
    assert Resource.parse_attributes(["int", "integer"]) == [{"int", :integer}]
    assert Resource.parse_attributes(["i", "interval"]) == [{"i", :interval}]
    assert Resource.parse_attributes(["m", "map"]) == [{"m", :map}]
    assert Resource.parse_attributes(["str", "string"]) == [{"str", :string}]
    assert Resource.parse_attributes(["t", "term"]) == [{"t", :term}]
    assert Resource.parse_attributes(["uuid", "uuid"]) == [{"uuid", :uuid}]
    assert Resource.parse_attributes(["utc", "utcdatetime"]) == [{"utc", :utcdatetime}]

    assert Resource.parse_attributes(["utcusec", "utcdatetimeusec"]) == [
             {"utcusec", :utcdatetimeusec}
           ]
  end

  test "generate_file" do
    assert Resource.generate_file({[], [], []}) == :error_missing_resource
    assert Resource.generate_file({[], ["1", "2"], ["invalid"]}) == :error_invalid_attribute
    assert Resource.generate_file({[], ["resource"], []}) == :error_missing_columns

    assert Resource.generate_file({[], ["user", "name"], []}) |> elem(0) =~ ":name, :string"

    assert Resource.generate_file({[], ["user", "name", "integer"], []}) |> elem(0) =~
             ":name, :integer"

    assert Resource.generate_file({[], ["user", "name"], []}) |> elem(0) =~ "Ash.User"

    assert Resource.generate_file({[{:api, "accounts"}], ["user", "name"], []}) |> elem(0) =~
             "Ash.Accounts.User"

    refute Resource.generate_file({[postgres: false], ["user", "name"], []}) |> elem(0) =~
             ~s/table("user")/

    assert Resource.generate_file({[postgres: true], ["user", "name"], []}) |> elem(0) =~
             ~s/table("user")/

    assert Resource.generate_file(
             {[postgres: true, table_name: "accounts"], ["user", "name"], []}
           )
           |> elem(0) =~ ~s/table("accounts")/

    refute Resource.generate_file({[graphql: false], ["user", "name"], []}) |> elem(0) =~
             ~s/graphql do\n/

    assert Resource.generate_file({[graphql: true], ["user", "name"], []}) |> elem(0) =~
             ~s/graphql do\n/

    refute Resource.generate_file({[json_api: false], ["user", "name"], []}) |> elem(0) =~
             ~s/json_api do\n/

    assert Resource.generate_file({[json_api: true], ["user", "name"], []}) |> elem(0) =~
             ~s/json_api do\n/

    refute Resource.generate_file({[guides: false], ["user", "name"], []}) |> elem(0) =~
             ~s/# attribute/

    assert Resource.generate_file({[guides: true], ["user", "name"], []}) |> elem(0) =~
             ~s/# attribute/

    refute Resource.generate_file({[timestamps: false], ["user", "name"], []}) |> elem(0) =~
             ~s/create_timestamp/

    assert Resource.generate_file({[timestamps: true], ["user", "name"], []}) |> elem(0) =~
             ~s/create_timestamp/

    refute Resource.generate_file({[policy_authorizer: false], ["user", "name"], []}) |> elem(0) =~
             ~s/policies do\n/

    assert Resource.generate_file({[policy_authorizer: true], ["user", "name"], []}) |> elem(0) =~
             ~s/policies do\n/
  end
end
