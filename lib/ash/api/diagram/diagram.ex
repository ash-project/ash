defmodule Ash.Api.Info.Diagram do
  @moduledoc """
  Generate Mermaid diagrams from a specified API.
  """

  def resources_with_attrs(api) do
    for resource <- Ash.Api.Info.resources(api) do
      {resource, Ash.Resource.Info.public_attributes(resource)}
    end
  end

  def normalise_relationships(api) do
    for resource <- Ash.Api.Info.resources(api) do
      for relationship <- Ash.Resource.Info.public_relationships(resource) do
        [relationship.source, relationship.destination]
        |> Enum.sort()
        |> List.to_tuple()
      end
    end
    |> Enum.flat_map(& &1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  def short_module(mod) do
    mod |> Module.split() |> List.last()
  end

  def aggregate_type(resource, aggregate) do
    attribute_type =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field).type
      end

    Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type)
  end

  def rel_type(:has_many), do: "|o--o{"
  def rel_type(:has_one), do: "|o--||"
  def rel_type(:many_to_many), do: "}o--o{"

  def short_type({:array, t}), do: "ArrayOf#{short_module(t)}"
  def short_type(t), do: short_module(t)

  def mermaid_er_diagram(api) do
    indent = "    "

    resources =
      for {resource, attrs} <- resources_with_attrs(api) do
        """
        #{indent}#{Ash.Resource.Info.trace_name(resource)} {
        #{Enum.join(for(attr <- attrs, do: "#{indent}#{indent}#{short_type(attr.type)} #{attr.name}"), "\n")}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, type, dest} <- normalise_relationships(api) do
        ~s(#{indent}#{Ash.Resource.Info.trace_name(src)} #{rel_type(type)} #{Ash.Resource.Info.trace_name(dest)} : "")
      end
      |> Enum.join("\n")

    """
    erDiagram
    #{resources}
    #{relationships}
    """
  end

  # def class_type(:has_many), do: "--*"
  # def class_type(:belongs_to), do: "*--"
  # def class_type(:has_one), do: "--"
  # def class_type(:many_to_many), do: "*--*"

  def class_short_type({:array, t}), do: "#{short_module(t)}[]"
  def class_short_type(t), do: short_module(t)

  def mermaid_class_diagram(api) do
    indent = "    "

    resources =
      for {resource, attrs} <- resources_with_attrs(api) do
        actions = Ash.Resource.Info.actions(resource)
        relationships = Ash.Resource.Info.public_relationships(resource)
        calcs = Ash.Resource.Info.public_calculations(resource)
        aggs = Ash.Resource.Info.public_aggregates(resource)

        """
        #{indent}class #{Ash.Resource.Info.trace_name(resource)} {
        #{Enum.join(for(attr <- attrs, do: "#{indent}#{indent}#{class_short_type(attr.type)} #{attr.name}"), "\n")}
        #{Enum.join(for(calc <- calcs, do: "#{indent}#{indent}#{class_short_type(calc.type)} #{calc.name}"), "\n")}
        #{Enum.join(for(agg <- aggs, do: "#{indent}#{indent}#{aggregate_type(resource, agg)} #{agg.name}"), "\n")}
        #{Enum.join(for(rel <- relationships, do: "#{indent}#{indent}#{Ash.Resource.Info.trace_name(rel.destination)}#{if rel.cardinality == :many, do: "[]", else: ""} #{rel.name}"), "\n")}
        #{Enum.join(for(action <- actions, do: "#{indent}#{indent}#{action.name}()"), "\n")}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, dest} <- normalise_relationships(api) do
        ~s(#{indent}#{Ash.Resource.Info.trace_name(src)} -- #{Ash.Resource.Info.trace_name(dest)})
      end
      |> Enum.join("\n")

    """
    classDiagram
    #{resources}
    #{relationships}
    """
  end
end
