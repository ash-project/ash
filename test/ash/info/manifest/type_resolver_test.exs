# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.TypeResolverTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.Generator.TypeResolver
  alias Ash.Info.Manifest.Type

  describe "resolve/2 primitives" do
    test "resolves Ash.Type.String" do
      result = TypeResolver.resolve(Ash.Type.String, [])
      assert %Type{kind: :string, name: "String"} = result
    end

    test "resolves Ash.Type.Integer" do
      result = TypeResolver.resolve(Ash.Type.Integer, [])
      assert %Type{kind: :integer, name: "Integer"} = result
    end

    test "resolves Ash.Type.Boolean" do
      result = TypeResolver.resolve(Ash.Type.Boolean, [])
      assert %Type{kind: :boolean, name: "Boolean"} = result
    end

    test "resolves Ash.Type.UUID" do
      result = TypeResolver.resolve(Ash.Type.UUID, [])
      assert %Type{kind: :uuid, name: "UUID"} = result
    end

    test "resolves Ash.Type.Date" do
      result = TypeResolver.resolve(Ash.Type.Date, [])
      assert %Type{kind: :date, name: "Date"} = result
    end

    test "resolves Ash.Type.UtcDatetime" do
      result = TypeResolver.resolve(Ash.Type.UtcDatetime, [])
      assert %Type{kind: :utc_datetime, name: "UtcDateTime"} = result
    end

    test "resolves Ash.Type.Decimal" do
      result = TypeResolver.resolve(Ash.Type.Decimal, [])
      assert %Type{kind: :decimal, name: "Decimal"} = result
    end

    test "resolves Ash.Type.Float" do
      result = TypeResolver.resolve(Ash.Type.Float, [])
      assert %Type{kind: :float, name: "Float"} = result
    end
  end

  describe "resolve/2 atom shorthands" do
    test "resolves :string" do
      assert %Type{kind: :string} = TypeResolver.resolve(:string, [])
    end

    test "resolves :integer" do
      assert %Type{kind: :integer} = TypeResolver.resolve(:integer, [])
    end

    test "resolves :boolean" do
      assert %Type{kind: :boolean} = TypeResolver.resolve(:boolean, [])
    end

    test "resolves :uuid" do
      assert %Type{kind: :uuid} = TypeResolver.resolve(:uuid, [])
    end

    # Regression: each of these used to resolve to `kind: :unknown` because
    # they were missing from @atom_primitives. `:file` and `:function` also
    # crashed `Module.split/1` in the fallback path because they are loaded
    # Erlang modules.
    test "resolves every atom shorthand Ash exposes" do
      assert %Type{kind: :uuid, name: "UUIDv7"} = TypeResolver.resolve(:uuid_v7, [])
      assert %Type{kind: :time_usec, name: "TimeUsec"} = TypeResolver.resolve(:time_usec, [])

      assert %Type{kind: :binary, name: "UrlEncodedBinary"} =
               TypeResolver.resolve(:url_encoded_binary, [])

      assert %Type{kind: :atom, name: "Module"} = TypeResolver.resolve(:module, [])
      assert %Type{kind: :term, name: "File"} = TypeResolver.resolve(:file, [])
      assert %Type{kind: :term, name: "Function"} = TypeResolver.resolve(:function, [])
      assert %Type{kind: :term, name: "Vector"} = TypeResolver.resolve(:vector, [])
    end

    test "atom shorthand and module form resolve to the same kind/name" do
      pairs = [
        {:uuid_v7, Ash.Type.UUIDv7},
        {:time_usec, Ash.Type.TimeUsec},
        {:url_encoded_binary, Ash.Type.UrlEncodedBinary},
        {:module, Ash.Type.Module},
        {:file, Ash.Type.File},
        {:function, Ash.Type.Function},
        {:vector, Ash.Type.Vector}
      ]

      for {shorthand, module} <- pairs do
        from_shorthand = TypeResolver.resolve(shorthand, [])
        from_module = TypeResolver.resolve(module, [])

        assert from_shorthand.kind == from_module.kind,
               "kind mismatch for #{inspect(shorthand)} vs #{inspect(module)}"

        assert from_shorthand.name == from_module.name,
               "name mismatch for #{inspect(shorthand)} vs #{inspect(module)}"
      end
    end
  end

  describe "resolve/2 arrays" do
    test "resolves {:array, :string}" do
      result = TypeResolver.resolve({:array, :string}, [])
      assert %Type{kind: :array, item_type: %Type{kind: :string}} = result
    end

    test "resolves {:array, Ash.Type.Integer}" do
      result = TypeResolver.resolve({:array, Ash.Type.Integer}, [])
      assert %Type{kind: :array, item_type: %Type{kind: :integer}} = result
    end

    test "passes items constraints to inner type" do
      result =
        TypeResolver.resolve({:array, Ash.Type.Integer}, items: [min: 0])

      assert %Type{kind: :array, item_type: %Type{kind: :integer, constraints: [min: 0]}} =
               result
    end
  end

  describe "resolve/2 enums" do
    test "resolves named enum type as type_ref" do
      result = TypeResolver.resolve(Ash.Test.Manifest.Todo.Status, [])
      assert %Type{kind: :type_ref, module: Ash.Test.Manifest.Todo.Status} = result
    end

    test "resolves atom with one_of constraint as inline enum" do
      result = TypeResolver.resolve(Ash.Type.Atom, one_of: [:low, :medium, :high])
      assert %Type{kind: :enum, values: [:low, :medium, :high]} = result
    end
  end

  describe "resolve/2 unions" do
    test "resolves union type" do
      constraints = [
        types: [
          text: [type: :string],
          number: [type: :integer]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Union, constraints)
      assert %Type{kind: :union, members: members} = result
      assert length(members) == 2
      assert Enum.any?(members, &(&1.name == :text))
      assert Enum.any?(members, &(&1.name == :number))
      assert Enum.all?(members, &Map.has_key?(&1, :description))
    end

    test "preserves member descriptions" do
      constraints = [
        types: [
          text: [type: :string, description: "Plain text content"],
          number: [type: :integer]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Union, constraints)
      text_member = Enum.find(result.members, &(&1.name == :text))
      number_member = Enum.find(result.members, &(&1.name == :number))
      assert text_member.description == "Plain text content"
      assert number_member.description == nil
    end
  end

  describe "resolve/2 maps" do
    test "resolves map with fields" do
      constraints = [
        fields: [
          name: [type: :string, allow_nil?: false],
          age: [type: :integer, allow_nil?: true]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Map, constraints)
      assert %Type{kind: :map, fields: fields} = result
      assert length(fields) == 2
      name_field = Enum.find(fields, &(&1.name == :name))
      assert name_field.allow_nil? == false
      assert %Type{kind: :string} = name_field.type
      assert name_field.description == nil
    end

    test "preserves field descriptions" do
      constraints = [
        fields: [
          name: [type: :string, description: "Display name"],
          age: [type: :integer]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Map, constraints)
      name_field = Enum.find(result.fields, &(&1.name == :name))
      age_field = Enum.find(result.fields, &(&1.name == :age))
      assert name_field.description == "Display name"
      assert age_field.description == nil
    end

    test "resolves map without fields" do
      result = TypeResolver.resolve(Ash.Type.Map, [])
      assert %Type{kind: :map, fields: nil} = result
    end
  end

  describe "resolve/2 keyword" do
    test "resolves keyword with fields" do
      constraints = [
        fields: [
          priority: [type: :integer, allow_nil?: false],
          category: [type: :string, allow_nil?: true]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Keyword, constraints)
      assert %Type{kind: :keyword, fields: fields} = result
      assert length(fields) == 2
    end
  end

  describe "resolve/2 tuple" do
    test "resolves tuple with fields" do
      constraints = [
        fields: [
          latitude: [type: :float, allow_nil?: false],
          longitude: [type: :float, allow_nil?: false]
        ]
      ]

      result = TypeResolver.resolve(Ash.Type.Tuple, constraints)
      assert %Type{kind: :tuple, element_types: elements} = result
      assert length(elements) == 2
    end
  end

  describe "resolve/2 struct" do
    test "resolves struct with resource instance_of" do
      constraints = [instance_of: Ash.Test.Manifest.User]
      result = TypeResolver.resolve(Ash.Type.Struct, constraints)
      assert %Type{kind: :resource, resource_module: Ash.Test.Manifest.User} = result
    end

    test "resolves struct with embedded resource instance_of" do
      constraints = [instance_of: Ash.Test.Manifest.TodoMetadata]
      result = TypeResolver.resolve(Ash.Type.Struct, constraints)
      assert %Type{resource_module: Ash.Test.Manifest.TodoMetadata} = result
      assert result.kind in [:resource, :embedded_resource]
    end
  end

  describe "resolve/2 embedded resources" do
    test "resolves embedded resource directly" do
      result = TypeResolver.resolve(Ash.Test.Manifest.TodoMetadata, [])
      assert %Type{resource_module: Ash.Test.Manifest.TodoMetadata} = result
    end
  end

  describe "resolve/2 additional primitives" do
    test "resolves Ash.Type.UrlEncodedBinary as binary" do
      result = TypeResolver.resolve(Ash.Type.UrlEncodedBinary, [])
      assert %Type{kind: :binary, name: "UrlEncodedBinary"} = result
    end

    test "resolves Ash.Type.Module as atom" do
      result = TypeResolver.resolve(Ash.Type.Module, [])
      assert %Type{kind: :atom, name: "Module"} = result
    end

    test "resolves Ash.Type.File as term" do
      result = TypeResolver.resolve(Ash.Type.File, [])
      assert %Type{kind: :term, name: "File"} = result
    end

    test "resolves Ash.Type.Function as term" do
      result = TypeResolver.resolve(Ash.Type.Function, [])
      assert %Type{kind: :term, name: "Function"} = result
    end

    test "resolves Ash.Type.Vector as term" do
      result = TypeResolver.resolve(Ash.Type.Vector, [])
      assert %Type{kind: :term, name: "Vector"} = result
    end

    test "resolves Ash.Type.DurationName as type_ref" do
      result = TypeResolver.resolve(Ash.Type.DurationName, [])
      assert %Type{kind: :type_ref, module: Ash.Type.DurationName} = result
    end
  end

  describe "resolve/2 nil and unknown" do
    test "resolves nil as unknown" do
      result = TypeResolver.resolve(nil, [])
      assert %Type{kind: :unknown} = result
    end
  end

  describe "unwrap_new_type/2" do
    test "unwraps NewType to subtype" do
      # Todo.Status is a NewType of Ash.Type.Atom
      {unwrapped, _constraints} =
        TypeResolver.unwrap_new_type(Ash.Test.Manifest.Todo.Status, [])

      # Should unwrap to the underlying type
      assert is_atom(unwrapped)
    end

    test "non-NewType returns unchanged" do
      {unwrapped, constraints} = TypeResolver.unwrap_new_type(Ash.Type.String, [])
      assert unwrapped == Ash.Type.String
      assert constraints == []
    end
  end

  describe "resolve_definition/1" do
    test "resolves enum to full definition with values" do
      result = TypeResolver.resolve_definition(Ash.Test.Manifest.Todo.Status)
      assert %Type{kind: :enum, module: Ash.Test.Manifest.Todo.Status, values: values} = result
      assert is_list(values)
      assert :pending in values
    end

    test "resolves Ash.Type.DurationName to full enum definition" do
      result = TypeResolver.resolve_definition(Ash.Type.DurationName)
      assert %Type{kind: :enum, module: Ash.Type.DurationName} = result
      assert :year in result.values
    end
  end

  describe "named_type_module?/1" do
    test "returns true for enum types" do
      assert TypeResolver.named_type_module?(Ash.Test.Manifest.Todo.Status)
    end

    test "returns true for Ash.Type.DurationName" do
      assert TypeResolver.named_type_module?(Ash.Type.DurationName)
    end

    test "returns false for primitives" do
      refute TypeResolver.named_type_module?(Ash.Type.String)
      refute TypeResolver.named_type_module?(Ash.Type.Integer)
    end

    test "returns false for resources" do
      refute TypeResolver.named_type_module?(Ash.Test.Manifest.Todo)
    end

    test "returns false for non-atoms" do
      refute TypeResolver.named_type_module?({:array, :string})
    end
  end
end
