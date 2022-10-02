defmodule Ash.Api.Info.Diagram do
  @moduledoc """
  Generate Mermaid diagrams from a specified API.
  """

  @indent "    "

  @default_opts indent: @indent

  defp resource_name(resource) do
    resource
    |> Ash.Resource.Info.short_name()
    |> to_string()
    |> Macro.camelize()
  end

  defp short_module(module) do
    module
    |> Module.split()
    |> List.last()
  end

  defp normalise_relationships(api) do
    for resource <- Ash.Api.Info.resources(api) do
      for relationship <- Ash.Resource.Info.relationships(resource) do
        [relationship.source, relationship.destination]
        |> Enum.sort()
        |> List.to_tuple()
      end
    end
    |> Enum.flat_map(& &1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp aggregate_type(resource, aggregate) do
    attribute_type =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field).type
      end

    Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type)
  end

  # default to one to one to just show connection
  defp rel_type, do: "||--||"

  defp short_type({:array, t}), do: "ArrayOf#{short_module(t)}"
  defp short_type(t), do: short_module(t)

  def mermaid_er_diagram(api, opts \\ @default_opts) do
    indent = opts[:indent] || @indent

    resources =
      for resource <- Ash.Api.Info.resources(api) do
        attrs = Ash.Resource.Info.public_attributes(resource)
        calcs = Ash.Resource.Info.public_calculations(resource)
        aggs = Ash.Resource.Info.public_aggregates(resource)

        contents =
          [
            join_template(attrs, indent, &"#{short_type(&1.type)} #{&1.name}"),
            join_template(calcs, indent, &"#{short_type(&1.type)} #{&1.name}"),
            join_template(aggs, indent, &"#{aggregate_type(resource, &1)} #{&1.name}")
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        """
        #{indent}#{resource_name(resource)} {
        #{contents}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, dest} <- normalise_relationships(api) do
        ~s(#{indent}#{resource_name(src)} #{rel_type()} #{resource_name(dest)} : "")
      end
      |> Enum.join("\n")

    """
    erDiagram
    #{resources}
    #{relationships}
    """
  end

  defp class_short_type({:array, t}), do: "#{short_module(t)}[]"
  defp class_short_type(t), do: short_module(t)

  defp join_template(list, indent, template_fn) do
    Enum.map_join(list, "\n", fn item -> "#{indent}#{indent}#{template_fn.(item)}" end)
  end

  def mermaid_class_diagram(api, opts \\ @default_opts) do
    indent = opts[:indent] || @indent

    resources =
      for resource <- Ash.Api.Info.resources(api) do
        attrs = Ash.Resource.Info.public_attributes(resource)
        calcs = Ash.Resource.Info.public_calculations(resource)
        aggs = Ash.Resource.Info.public_aggregates(resource)
        actions = Ash.Resource.Info.actions(resource)
        relationships = Ash.Resource.Info.public_relationships(resource)

        contents =
          [
            join_template(attrs, indent, &"#{class_short_type(&1.type)} #{&1.name}"),
            join_template(calcs, indent, &"#{class_short_type(&1.type)} #{&1.name}"),
            join_template(aggs, indent, &"#{aggregate_type(resource, &1)} #{&1.name}"),
            join_template(
              relationships,
              indent,
              &"#{resource_name(&1.destination)}#{if &1.cardinality == :many, do: "[]", else: ""} #{&1.name}"
            ),
            join_template(actions, indent, &"#{&1.name}()")
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        """
        #{indent}class #{resource_name(resource)} {
        #{contents}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, dest} <- normalise_relationships(api) do
        ~s(#{indent}#{resource_name(src)} -- #{resource_name(dest)})
      end
      |> Enum.join("\n")

    """
    classDiagram
    #{resources}
    #{relationships}
    """
  end
end
