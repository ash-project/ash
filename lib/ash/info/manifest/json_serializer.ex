# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.JsonSerializer do
  @moduledoc """
  Serializes `%Ash.Info.Manifest{}` structs to JSON.

  Recursively converts structs to maps, converting module atoms to strings
  and omitting nil fields.
  """

  @doc """
  Serialize an `%Ash.Info.Manifest{}` to a JSON string.
  """
  @spec to_json(Ash.Info.Manifest.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def to_json(%Ash.Info.Manifest{} = spec, opts \\ []) do
    pretty? = Keyword.get(opts, :pretty, false)

    map = to_map(spec)

    json_opts = if pretty?, do: [pretty: true], else: []

    {:ok, Jason.encode!(map, json_opts)}
  rescue
    e -> {:error, Exception.message(e)}
  end

  @doc """
  Convert an `%Ash.Info.Manifest{}` to a plain map (suitable for JSON encoding).
  """
  @spec to_map(Ash.Info.Manifest.t()) :: map()
  def to_map(%Ash.Info.Manifest{} = spec) do
    %{
      "schema_version" => Ash.Info.Manifest.schema_version(),
      "resources" => Enum.map(spec.resources, &serialize_resource/1),
      "types" => Enum.map(spec.types, &serialize_type/1),
      "entrypoints" => Enum.map(spec.entrypoints, &serialize_entrypoint/1)
    }
    |> put_if_present(
      "filter_capabilities",
      serialize_filter_capabilities(spec.filter_capabilities)
    )
    |> put_if_present(
      "sort_capabilities",
      serialize_sort_capabilities(spec.sort_capabilities)
    )
  end

  defp serialize_filter_capabilities(nil), do: nil

  defp serialize_filter_capabilities(%Ash.Info.Manifest.FilterCapabilities{} = caps) do
    %{
      "operators" => Enum.map(caps.operators, &serialize_operator/1),
      "functions" => Enum.map(caps.functions, &serialize_function/1),
      "custom_expressions" => Enum.map(caps.custom_expressions, &serialize_custom_expression/1),
      "boolean_connectives" => Enum.map(caps.boolean_connectives, &to_string/1),
      "predicate_operators" => Enum.map(caps.predicate_operators, &to_string/1),
      "predicate_functions" => Enum.map(caps.predicate_functions, &to_string/1),
      "predicate_custom_expressions" => Enum.map(caps.predicate_custom_expressions, &to_string/1)
    }
  end

  defp serialize_sort_capabilities(nil), do: nil

  defp serialize_sort_capabilities(%Ash.Info.Manifest.SortCapabilities{} = caps) do
    %{"directions" => Enum.map(caps.directions, &to_string/1)}
  end

  defp serialize_operator(%Ash.Info.Manifest.Operator{} = op) do
    %{
      "name" => to_string(op.name),
      "module" => module_to_string(op.module),
      "aliases" => Enum.map(op.aliases || [], &to_string/1),
      "predicate" => op.predicate?,
      "signatures" => Enum.map(op.signatures || [], &serialize_signature/1),
      "returns" => serialize_returns(op.returns)
    }
    |> put_if_present("description", op.description)
  end

  defp serialize_function(%Ash.Info.Manifest.Function{} = fun) do
    %{
      "name" => to_string(fun.name),
      "module" => module_to_string(fun.module),
      "predicate" => fun.predicate?,
      "signatures" => serialize_function_signatures(fun.signatures),
      "returns" => serialize_returns(fun.returns)
    }
    |> put_if_present("description", fun.description)
    |> put_if_present("data_layer_module", module_to_string(fun.data_layer_module))
  end

  defp serialize_custom_expression(%Ash.Info.Manifest.CustomExpression{} = ce) do
    %{
      "name" => to_string(ce.name),
      "module" => module_to_string(ce.module),
      "predicate" => ce.predicate?,
      "signatures" => Enum.map(ce.signatures || [], &serialize_signature/1)
    }
    |> put_if_present("description", ce.description)
  end

  defp serialize_function_signatures(:var_args), do: "var_args"

  defp serialize_function_signatures(sigs) when is_list(sigs),
    do: Enum.map(sigs, &serialize_signature/1)

  defp serialize_signature(%Ash.Info.Manifest.ArgumentSignature{args: args}) do
    %{"args" => Enum.map(args, &serialize_arg_spec/1)}
  end

  defp serialize_arg_spec(%{kind: kind} = arg) do
    %{"kind" => to_string(kind)}
    |> put_if_present("type_ref", module_to_string(arg[:type_ref]))
    |> put_if_present("of", serialize_arg_of(arg[:of]))
    |> put_if_present(
      "constraints",
      if(arg[:constraints] in [nil, []], do: nil, else: inspect(arg[:constraints]))
    )
  end

  defp serialize_arg_of(nil), do: nil
  defp serialize_arg_of(%{kind: _} = inner), do: serialize_arg_spec(inner)

  defp serialize_returns(:unknown), do: "unknown"
  defp serialize_returns(nil), do: nil
  defp serialize_returns(%{kind: _} = arg), do: serialize_arg_spec(arg)

  defp serialize_resource(%Ash.Info.Manifest.Resource{} = resource) do
    %{
      "name" => resource.name,
      "module" => module_to_string(resource.module),
      "embedded" => resource.embedded?,
      "primary_key" => Enum.map(resource.primary_key || [], &to_string/1),
      "fields" => serialize_named_map(resource.fields, &serialize_field/1),
      "relationships" => serialize_named_map(resource.relationships, &serialize_relationship/1)
    }
    |> put_if_present("description", resource.description)
    |> put_if_present("multitenancy", serialize_multitenancy(resource.multitenancy))
  end

  defp serialize_entrypoint(%Ash.Info.Manifest.Entrypoint{} = entrypoint) do
    # config is omitted from JSON — it carries internal extension-specific structs
    %{
      "resource" => module_to_string(entrypoint.resource),
      "action" => serialize_action(entrypoint.action)
    }
  end

  defp serialize_field(%Ash.Info.Manifest.Field{} = field) do
    base = %{
      "kind" => to_string(field.kind),
      "type" => serialize_type(field.type),
      "allow_nil" => field.allow_nil?,
      "writable" => field.writable?,
      "has_default" => field.has_default?,
      "filterable" => field.filterable?,
      "sortable" => field.sortable?,
      "primary_key" => field.primary_key?,
      "sensitive" => field.sensitive?,
      "select_by_default" => field.select_by_default?
    }

    base
    |> put_if_present("description", field.description)
    |> put_if_present("arguments", serialize_arguments_list(field.arguments))
    |> put_if_present("aggregate_kind", serialize_atom(field.aggregate_kind))
    |> put_if_present("filter_operators", serialize_applicable_list(field.filter_operators))
    |> put_if_present("filter_functions", serialize_applicable_list(field.filter_functions))
    |> put_if_present(
      "filter_custom_expressions",
      serialize_applicable_list(field.filter_custom_expressions)
    )
  end

  defp serialize_applicable_list(nil), do: nil

  defp serialize_applicable_list(list) when is_list(list) do
    Enum.map(list, &serialize_applicable/1)
  end

  defp serialize_applicable(%Ash.Info.Manifest.ApplicableOperator{name: name, rhs: rhs}) do
    %{"name" => to_string(name), "rhs" => serialize_rhs(rhs)}
  end

  defp serialize_applicable(%Ash.Info.Manifest.ApplicableFunction{name: name, rhs: rhs}) do
    %{"name" => to_string(name), "rhs" => serialize_rhs(rhs)}
  end

  defp serialize_applicable(%Ash.Info.Manifest.ApplicableCustomExpression{
         name: name,
         rhs: rhs
       }) do
    %{"name" => to_string(name), "rhs" => serialize_rhs(rhs)}
  end

  defp serialize_rhs(:same), do: "same"
  defp serialize_rhs(:any), do: "any"

  defp serialize_rhs({:concrete, ref}) when is_atom(ref),
    do: %{"concrete" => module_to_string(ref)}

  defp serialize_rhs({:array, inner}), do: %{"array" => serialize_rhs(inner)}
  defp serialize_rhs(_), do: "any"

  defp serialize_relationship(%Ash.Info.Manifest.Relationship{} = rel) do
    %{
      "type" => to_string(rel.type),
      "cardinality" => to_string(rel.cardinality),
      "destination" => module_to_string(rel.destination),
      "allow_nil" => rel.allow_nil?,
      "filterable" => rel.filterable?,
      "sortable" => rel.sortable?
    }
    |> put_if_present("description", rel.description)
  end

  defp serialize_action(%Ash.Info.Manifest.Action{} = action) do
    %{
      "type" => to_string(action.type),
      "primary" => action.primary?,
      "get" => action.get?,
      "inputs" => Enum.map(action.inputs || [], &serialize_argument/1),
      "metadata" => Enum.map(action.metadata || [], &serialize_metadata/1)
    }
    |> put_if_present("description", action.description)
    |> put_if_present("returns", serialize_type(action.returns))
    |> put_if_present("pagination", serialize_pagination(action.pagination))
  end

  defp serialize_type(nil), do: nil

  defp serialize_type(%Ash.Info.Manifest.Type{} = type) do
    base = %{
      "kind" => to_string(type.kind),
      "name" => type.name,
      "allow_nil" => type.allow_nil?
    }

    base
    |> put_if_present("module", module_to_string(type.module))
    |> put_if_present("values", serialize_atom_list(type.values))
    |> put_if_present("members", serialize_members(type.members))
    |> put_if_present("resource_module", module_to_string(type.resource_module))
    |> put_if_present("fields", serialize_type_fields(type.fields))
    |> put_if_present("instance_of", module_to_string(type.instance_of))
    |> put_if_present("item_type", serialize_type(type.item_type))
    |> put_if_present("element_types", serialize_type_fields(type.element_types))
    |> put_if_present("resource", serialize_nested_resource(type.resource))
  end

  defp serialize_nested_resource(nil), do: nil

  defp serialize_nested_resource(%Ash.Info.Manifest.Resource{} = resource),
    do: serialize_resource(resource)

  defp serialize_argument(%Ash.Info.Manifest.Argument{} = arg) do
    %{
      "name" => to_string(arg.name),
      "type" => serialize_type(arg.type),
      "allow_nil" => arg.allow_nil?,
      "has_default" => arg.has_default?,
      "required" => arg.required?,
      "sensitive" => arg.sensitive?
    }
    |> put_if_present("description", arg.description)
  end

  defp serialize_metadata(%Ash.Info.Manifest.Metadata{} = meta) do
    %{
      "name" => to_string(meta.name),
      "type" => serialize_type(meta.type),
      "allow_nil" => meta.allow_nil?
    }
    |> put_if_present("description", meta.description)
  end

  defp serialize_pagination(nil), do: nil

  defp serialize_pagination(%Ash.Info.Manifest.Pagination{} = page) do
    %{
      "offset" => page.offset?,
      "keyset" => page.keyset?,
      "required" => page.required?,
      "countable" => page.countable?,
      "default_limit" => page.default_limit,
      "max_page_size" => page.max_page_size
    }
  end

  defp serialize_multitenancy(nil), do: nil

  defp serialize_multitenancy(%{} = mt) do
    %{
      "strategy" => to_string(mt.strategy),
      "global" => mt.global?,
      "attribute" => serialize_atom(mt.attribute)
    }
  end

  defp serialize_named_map(map, serialize_fn) when is_map(map) do
    Map.new(map, fn {name, value} ->
      {to_string(name), serialize_fn.(value)}
    end)
  end

  defp serialize_arguments_list(nil), do: nil
  defp serialize_arguments_list([]), do: nil
  defp serialize_arguments_list(args), do: Enum.map(args, &serialize_argument/1)

  defp serialize_members(nil), do: nil

  defp serialize_members(members) do
    Enum.map(members, fn member ->
      %{
        "name" => to_string(member.name),
        "type" => serialize_type(member.type)
      }
      |> put_if_present("description", Map.get(member, :description))
    end)
  end

  defp serialize_type_fields(nil), do: nil

  defp serialize_type_fields(fields) do
    Enum.map(fields, fn field ->
      %{
        "name" => to_string(field.name),
        "type" => serialize_type(field.type),
        "allow_nil" => field.allow_nil?
      }
      |> put_if_present("description", Map.get(field, :description))
    end)
  end

  defp serialize_atom_list(nil), do: nil
  defp serialize_atom_list(list), do: Enum.map(list, &to_string/1)

  defp serialize_atom(nil), do: nil
  defp serialize_atom(atom) when is_atom(atom), do: to_string(atom)

  defp module_to_string(nil), do: nil

  defp module_to_string(module) when is_atom(module) do
    case Atom.to_string(module) do
      "Elixir." <> _ -> module |> Module.split() |> Enum.join(".")
      other -> other
    end
  end

  defp module_to_string(other), do: inspect(other)

  defp put_if_present(map, _key, nil), do: map
  defp put_if_present(map, key, value), do: Map.put(map, key, value)
end
