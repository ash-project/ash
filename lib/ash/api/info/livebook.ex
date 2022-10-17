defmodule Ash.Api.Info.Livebook do
  @moduledoc """
  Generate a Livebook from a specified API.
  """

  # Strip Elixir off front of module
  defp module_name(module) do
    module
    |> Module.split()
    |> Enum.join(".")
  end

  # TODO: move to Ash.Resource.Info as it's also used in diagram
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

  def overview(apis) do
    """
    #{for api <- apis, do: api_section(api)}
    """
  end

  def api_section(api) do
    """
    # API #{module_name(api)}

    ## Class Diagram

    ```mermaid
    #{Ash.Api.Info.Diagram.mermaid_class_diagram(api) |> String.trim()}
    ```

    ## ER Diagram

    ```mermaid
    #{Ash.Api.Info.Diagram.mermaid_er_diagram(api) |> String.trim()}
    ```

    ## Resources

    #{for resource <- Ash.Api.Info.resources(api) do
      """
      - [#{resource_name(resource)}](##{resource_name(resource) |> String.downcase()})
      """
    end}
    #{for resource <- Ash.Api.Info.resources(api) do
      resource_section(resource)
    end |> Enum.join("\n")}
    """
  end

  def resource_section(resource) do
    """
    ## #{resource_name(resource)}

    #{Ash.Resource.Info.description(resource)}

    ### Attributes

    #{attr_header() |> String.trim()}
    #{for attr <- Ash.Resource.Info.attributes(resource) do
      attr_section(attr)
    end |> Enum.join("\n")}

    ### Actions

    #{action_header() |> String.trim()}
    #{for action <- Ash.Resource.Info.actions(resource) do
      action_section(action)
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
    | Name | Type | Description | Args |
    | ---- | ---- | ----------- | ---- |
    """
  end

  def action_section(action) do
    "| **#{action.name}** | _#{action.type}_ | #{action.description} | <ul>#{action_arg_section(action)}</ul> |"
  end

  def action_arg_section(action) do
    for arg <- action.arguments do
      "<li><b>#{arg.name}</b> <i>#{class_short_type(arg.type)}</i> #{arg.description}</li>"
    end
  end
end
