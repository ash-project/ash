defmodule Ash.Domain.Info.Diagram do
  @moduledoc """
  Generate Mermaid diagrams from a specified domain.

  ## Limitations

  We can't easily model Ash relationships with Mermaid diagrams
  because they are unidirectional and could be asymmetric.
  Mermaid assumes symmetrical, bidirectional relationships.
  If we try to model all unidirectional relationships as separate
  lines in the diagram it gets very hard to read very quickly.
  """

  @indent "    "
  @show_private? false
  @argument_print_limit 4

  @default_opts indent: @indent, show_private?: @show_private?

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

  defp normalise_relationships(domain) do
    for resource <- Ash.Domain.Info.resources(domain) do
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

  defp aggregate_type(_resource, %{kind: :custom, type: type}) do
    short_type(type)
  end

  defp aggregate_type(resource, aggregate) do
    attribute =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field)
      end

    attribute_type =
      if attribute do
        attribute.type
      end

    attribute_constraints =
      if attribute do
        attribute.constraints
      end

    {:ok, type, _constraints} =
      Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type, attribute_constraints)

    short_type(type)
  end

  # default to one to one to just show connection
  defp rel_type, do: "||--||"

  defp short_type({:array, t}), do: "ArrayOf#{short_module(t)}"
  defp short_type(t), do: short_module(t)

  @doc """
  Generates a Mermaid Entity Relationship Diagram for a given domain.

  Shows only public attributes, calculations, aggregates and actions.
  Shows a one-to-one line for relationships as enumerating all unidirectional
  relationships is far too noisy.
  """
  def mermaid_er_diagram(domain, opts \\ @default_opts) do
    indent = opts[:indent] || @indent
    show_private? = Access.get(opts, :show_private?, @show_private?)

    resources =
      for resource <- Ash.Domain.Info.resources(domain) do
        {attrs, calcs, aggs} =
          if show_private? do
            {
              Ash.Resource.Info.attributes(resource),
              Ash.Resource.Info.calculations(resource),
              Ash.Resource.Info.aggregates(resource)
            }
          else
            {
              Ash.Resource.Info.public_attributes(resource),
              Ash.Resource.Info.public_calculations(resource),
              Ash.Resource.Info.public_aggregates(resource)
            }
          end

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
      for {src, dest} <- normalise_relationships(domain) do
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

  defp list_arguments_and_attributes(resource, action) do
    arguments = action.arguments

    attributes =
      case action do
        %{type: :action} -> []
        %{type: :read} -> []
        %{type: :destroy, soft?: false} -> []
        %{accept: accept} -> accept |> Enum.map(&Ash.Resource.Info.attribute(resource, &1))
      end

    list_arguments(Enum.uniq_by(arguments ++ attributes, & &1.name))
  end

  defp list_arguments(arguments) when length(arguments) > @argument_print_limit do
    {displayed_args, _rest} = Enum.split(arguments, @argument_print_limit)
    list_arguments(displayed_args) <> ", ..."
  end

  defp list_arguments(arguments) do
    arguments
    |> Enum.map_join(", ", &"#{class_short_type(&1.type)} #{&1.name}")
  end

  @doc """
  Generates a Mermaid Class Diagram for a given domain.

  Shows only public attributes, calculations, aggregates and actions.
  Shows a connecting line for relationships with the type of relationship
  indicated in the attribute list.
  """
  def mermaid_class_diagram(domain, opts \\ @default_opts) do
    indent = opts[:indent] || @indent
    show_private? = Access.get(opts, :show_private?, @show_private?)

    resources =
      for resource <- Ash.Domain.Info.resources(domain) do
        actions = Ash.Resource.Info.actions(resource)

        {attrs, calcs, aggs, relationships} =
          if show_private? do
            {
              Ash.Resource.Info.attributes(resource),
              Ash.Resource.Info.calculations(resource),
              Ash.Resource.Info.aggregates(resource),
              Ash.Resource.Info.relationships(resource)
            }
          else
            {Ash.Resource.Info.public_attributes(resource),
             Ash.Resource.Info.public_calculations(resource),
             Ash.Resource.Info.public_aggregates(resource),
             Ash.Resource.Info.public_relationships(resource)}
          end

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
            join_template(
              actions,
              indent,
              &"#{&1.name}(#{list_arguments_and_attributes(resource, &1)})"
            )
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
      for {src, dest} <- normalise_relationships(domain) do
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
