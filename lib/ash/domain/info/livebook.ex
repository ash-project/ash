defmodule Ash.Domain.Info.Livebook do
  @moduledoc """
  Generate a Livebook from a specified domain.
  """

  # Strip Elixir off front of module
  defp module_name(module) do
    module
    |> Module.split()
    |> Enum.join(".")
  end

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

  defp class_short_type({:array, t}), do: "#{short_module(t)}[]"
  defp class_short_type(t), do: short_module(t)

  def overview(domains) do
    """
    # Domain Documentation

    #{for domain <- domains, do: domain_section(domain)}
    """
  end

  def domain_section(domain) do
    """
    ## Domain #{module_name(domain)}

    ### Class Diagram

    ```mermaid
    #{Ash.Domain.Info.Diagram.mermaid_class_diagram(domain) |> String.trim()}
    ```

    ### ER Diagram

    ```mermaid
    #{Ash.Domain.Info.Diagram.mermaid_er_diagram(domain) |> String.trim()}
    ```

    ### Resources

    #{for resource <- Ash.Domain.Info.resources(domain) do
      """
      - [#{resource_name(resource)}](##{resource_name(resource) |> String.downcase()})
      """
    end}
    #{for resource <- Ash.Domain.Info.resources(domain) do
      resource_section(resource)
    end |> Enum.join("\n")}
    """
  end

  def resource_section(resource) do
    """
    ### #{resource_name(resource)}

    #{Ash.Resource.Info.description(resource)}

    #### Attributes

    #{attr_header() |> String.trim()}
    #{for attr <- Ash.Resource.Info.attributes(resource) do
      attr_section(attr)
    end |> Enum.join("\n")}

    #### Actions

    #{action_header() |> String.trim()}
    #{for action <- Ash.Resource.Info.actions(resource) do
      action_section(resource, action)
    end |> Enum.join("\n")}
    """
  end

  def attr_header do
    """
    | Name | Type | Description |
    | ---- | ---- | ----------- |
    """
  end

  def attr_section(attr) do
    "| **#{attr.name}** | #{class_short_type(attr.type)} | #{attr.description} |"
  end

  def action_header do
    """
    | Name | Type | Input | Description |
    | ---- | ---- | ----- | ----------- |
    """
  end

  def action_section(resource, action) do
    "| **#{action.name}** | _#{action.type}_ | <ul>#{action_input_section(resource, action)}</ul> | #{action.description} |"
  end

  def action_input_section(resource, action) do
    attributes =
      if action.type == :destroy && !action.soft? do
        []
      else
        action
        |> Map.get(:accept, [])
        |> Enum.map(&Ash.Resource.Info.attribute(resource, &1))
      end

    for input <- Enum.uniq_by(action.arguments ++ attributes, & &1.name) do
      description =
        case input do
          %Ash.Resource.Attribute{} ->
            "attribute"

          %Ash.Resource.Actions.Argument{description: description} ->
            description
        end

      "<li><b>#{input.name}</b> <i>#{class_short_type(input.type)}</i> #{description}</li>"
    end
  end
end
