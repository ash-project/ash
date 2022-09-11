defmodule Ash.Api.Info.Diagram do
  # possible options:
  # - show attrs
  # - show private attrs
  # - show private relships
  # - show calcs, aggs

  def resources_with_attrs(api) do
    for resource <- Ash.Api.Info.resources(api) do
      {resource, Ash.Resource.Info.public_attributes(resource)}
    end
  end

  def normalise_relationships(api) do
    for resource <- Ash.Api.Info.resources(api) do
      for relationship <- Ash.Resource.Info.public_relationships(resource) do
        # All relationships will be expressed from both sides and therefore appear twice
        # Normalise them so we can dedup
        case relationship.type do
          :belongs_to ->
            {relationship.destination, :has_many, relationship.source}

          :many_to_many ->
            if relationship.destination <= relationship.source do
              {relationship.destination, :many_to_many, relationship.source}
            else
              {relationship.source, :many_to_many, relationship.destination}
            end

          _ ->
            {relationship.source, relationship.type, relationship.destination}
        end
      end
    end
    |> Enum.flat_map(& &1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  def short_module(mod) do
    mod |> Module.split() |> List.last()
  end

  def type(:has_many), do: "|o--o{"
  def type(:has_one), do: "|o--||"
  def type(:many_to_many), do: "}o--o{"

  def short_type({:array, t}), do: "[#{short_module(t)}]"
  def short_type(t), do: short_module(t)

  def mermaid(api) do
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
        ~s(#{indent}#{Ash.Resource.Info.trace_name(src)} #{type(type)} #{Ash.Resource.Info.trace_name(dest)} : "")
      end
      |> Enum.join("\n")

    """
    erDiagram
    #{resources}
    #{relationships}
    """
  end
end
