# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.JsonSerializerTest do
  # async: false because the top-level capabilities describe blocks mutate
  # Application.put_env(:ash, :custom_expressions, ...) in setup.
  use ExUnit.Case, async: false

  alias Ash.Info.Manifest.JsonSerializer

  describe "to_json/2" do
    test "serializes a full spec to valid JSON" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)

      assert is_binary(json)
      assert {:ok, decoded} = Jason.decode(json)
      assert decoded["schema_version"] == "1.0.0"
      assert is_list(decoded["resources"])
    end

    test "pretty option produces formatted JSON" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec, pretty: true)

      assert String.contains?(json, "\n")
    end

    test "resources have expected structure" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))
      assert todo != nil
      assert is_binary(todo["module"])
      assert is_boolean(todo["embedded"])
      assert is_list(todo["primary_key"])
      assert is_map(todo["fields"])
      assert is_map(todo["relationships"])
      refute Map.has_key?(todo, "actions")
    end

    test "fields have expected structure" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))
      title = todo["fields"]["title"]

      assert title["kind"] == "attribute"
      assert title["type"]["kind"] == "string"
      assert title["allow_nil"] == false
      assert is_boolean(title["writable"])
      assert is_boolean(title["primary_key"])
    end

    test "relationships have expected structure" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))
      user_rel = todo["relationships"]["user"]

      assert user_rel["type"] == "belongs_to"
      assert user_rel["cardinality"] == "one"
      assert is_binary(user_rel["destination"])
    end

    test "entrypoints have expected structure" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      assert is_list(decoded["entrypoints"])
      assert decoded["entrypoints"] != []

      todo_read =
        Enum.find(decoded["entrypoints"], fn e ->
          String.ends_with?(e["resource"], "Todo") and e["action"]["type"] == "read"
        end)

      assert todo_read != nil
      assert is_binary(todo_read["resource"])
      assert is_boolean(todo_read["action"]["primary"])
      assert is_list(todo_read["action"]["inputs"])
    end

    test "pagination is serialized in entrypoints" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo_read =
        Enum.find(decoded["entrypoints"], fn e ->
          String.ends_with?(e["resource"], ".Todo") and
            e["action"]["type"] == "read" and
            e["action"]["primary"] == true
        end)

      assert todo_read != nil
      assert todo_read["action"]["pagination"] != nil
      assert is_boolean(todo_read["action"]["pagination"]["offset"])
      assert is_boolean(todo_read["action"]["pagination"]["keyset"])
    end

    test "generic action returns type is serialized" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))
      bulk_complete = todo["actions"]["bulk_complete"]

      if bulk_complete do
        assert bulk_complete["returns"] != nil
        assert bulk_complete["returns"]["kind"] == "array"
      end
    end

    test "nil fields are omitted from JSON" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))

      # Attribute fields should not have "arguments" key (only calculations have that)
      title = todo["fields"]["title"]
      refute Map.has_key?(title, "arguments")
    end

    test "module atoms are converted to strings" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json} = JsonSerializer.to_json(spec)
      {:ok, decoded} = Jason.decode(json)

      todo = Enum.find(decoded["resources"], &(&1["name"] == "Todo"))
      assert is_binary(todo["module"])
      assert String.contains?(todo["module"], "Todo")
    end
  end

  describe "to_map/1" do
    test "converts spec to plain map" do
      {:ok, spec} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      map = JsonSerializer.to_map(spec)

      assert is_map(map)
      assert map["schema_version"] == "1.0.0"
      assert is_list(map["resources"])
    end
  end

  describe "to_map/1 top-level filter/sort capabilities" do
    setup do
      original = Application.get_env(:ash, :custom_expressions, [])

      Application.put_env(:ash, :custom_expressions, [
        Ash.Test.Expressions.JaroDistance
      ])

      on_exit(fn -> Application.put_env(:ash, :custom_expressions, original) end)

      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, manifest: manifest, json: Ash.Info.Manifest.JsonSerializer.to_map(manifest)}
    end

    test "includes filter_capabilities", %{json: json} do
      assert %{"filter_capabilities" => caps} = json
      assert is_list(caps["operators"])
      assert is_list(caps["functions"])
      assert is_list(caps["custom_expressions"])
      assert caps["boolean_connectives"] == ["and", "or", "not"]
      assert is_list(caps["predicate_operators"])
      assert is_list(caps["predicate_functions"])
      assert is_list(caps["predicate_custom_expressions"])
    end

    test "includes sort_capabilities", %{json: json} do
      assert %{"sort_capabilities" => sort_caps} = json

      assert sort_caps["directions"] == [
               "asc",
               "desc",
               "asc_nils_first",
               "asc_nils_last",
               "desc_nils_first",
               "desc_nils_last"
             ]
    end

    test "each operator entry has name, module, predicate, signatures, returns", %{json: json} do
      eq =
        Enum.find(
          json["filter_capabilities"]["operators"],
          &(&1["module"] == "Ash.Query.Operator.Eq")
        )

      assert eq["name"] == to_string(Ash.Query.Operator.Eq.operator())
      assert eq["predicate"] == true
      assert is_list(eq["signatures"])
    end

    test "custom expression entries have name, module, predicate, signatures", %{json: json} do
      jaro =
        Enum.find(
          json["filter_capabilities"]["custom_expressions"],
          &(&1["module"] == "Ash.Test.Expressions.JaroDistance")
        )

      assert jaro["name"] == "jaro_distance"
      assert jaro["predicate"] == false

      assert [%{"args" => [a, b]}] = jaro["signatures"]
      assert a["kind"] == "concrete"
      assert a["type_ref"] == "Ash.Type.String"
      assert b["type_ref"] == "Ash.Type.String"
    end

    test "the JSON round-trips via Jason without errors", %{manifest: manifest} do
      assert {:ok, json_str} = Ash.Info.Manifest.JsonSerializer.to_json(manifest)
      assert {:ok, parsed} = Jason.decode(json_str)
      assert is_map(parsed["filter_capabilities"])
      assert is_map(parsed["sort_capabilities"])
    end

    test "builtin function entries omit data_layer_module", %{json: json} do
      contains =
        Enum.find(
          json["filter_capabilities"]["functions"],
          &(&1["module"] == "Ash.Query.Function.Contains")
        )

      refute Map.has_key?(contains, "data_layer_module")
    end

    test "data-layer-sourced function entries include data_layer_module" do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)

      manifest = %{
        manifest
        | filter_capabilities: %{
            manifest.filter_capabilities
            | functions:
                manifest.filter_capabilities.functions ++
                  [
                    %Ash.Info.Manifest.Function{
                      name: :fake_ilike,
                      module: Ash.Test.Manifest.FakeDataLayerFunction,
                      predicate?: true,
                      signatures: [],
                      returns: :unknown,
                      data_layer_module: Ash.Test.Manifest.FakeDataLayer
                    }
                  ]
          }
      }

      json = Ash.Info.Manifest.JsonSerializer.to_map(manifest)

      fake = Enum.find(json["filter_capabilities"]["functions"], &(&1["name"] == "fake_ilike"))

      assert fake["data_layer_module"] == "Ash.Test.Manifest.FakeDataLayer"
    end
  end

  describe "to_map/1 per-field filter operators/functions" do
    setup do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      json = Ash.Info.Manifest.JsonSerializer.to_map(manifest)
      {:ok, manifest: manifest, json: json}
    end

    test "filterable fields include filter_operators and filter_functions", %{json: json} do
      todo = Enum.find(json["resources"], &(&1["name"] == "Todo"))
      title = todo["fields"]["title"]

      assert is_list(title["filter_operators"])
      assert is_list(title["filter_functions"])

      eq_symbol = to_string(Ash.Query.Operator.Eq.operator())
      assert Enum.any?(title["filter_operators"], &(&1["name"] == eq_symbol))
    end

    test "filter_operators entries are {name, rhs} records", %{json: json} do
      todo = Enum.find(json["resources"], &(&1["name"] == "Todo"))
      title = todo["fields"]["title"]

      assert Enum.all?(title["filter_operators"], fn entry ->
               is_map(entry) and is_binary(entry["name"]) and Map.has_key?(entry, "rhs")
             end)

      eq = Enum.find(title["filter_operators"], &(&1["name"] == "=="))
      assert eq["rhs"] == "same"

      is_nil_entry = Enum.find(title["filter_operators"], &(&1["name"] == "is_nil"))
      assert is_nil_entry["rhs"] == %{"concrete" => "Ash.Type.Boolean"}

      in_entry = Enum.find(title["filter_operators"], &(&1["name"] == "in"))
      assert in_entry["rhs"] == %{"array" => "same"}
    end

    test "filter_custom_expressions is present on filterable fields", %{json: json} do
      todo = Enum.find(json["resources"], &(&1["name"] == "Todo"))
      title = todo["fields"]["title"]

      assert is_list(title["filter_custom_expressions"])
    end

    test "non-filterable fields omit filter_operators and filter_functions", %{json: json} do
      for resource <- json["resources"], {_name, field} <- resource["fields"] do
        if field["filterable"] == false do
          refute Map.has_key?(field, "filter_operators")
          refute Map.has_key?(field, "filter_functions")
          refute Map.has_key?(field, "filter_custom_expressions")
        end
      end
    end
  end

  describe "to_map/1 relationship filterable/sortable booleans" do
    setup do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, json: Ash.Info.Manifest.JsonSerializer.to_map(manifest)}
    end

    test "relationships carry filterable + sortable booleans and cardinality", %{json: json} do
      for resource <- json["resources"], {_name, rel} <- resource["relationships"] do
        assert is_boolean(rel["filterable"])
        assert is_boolean(rel["sortable"])
        assert rel["cardinality"] in ["one", "many"]
      end
    end
  end
end
