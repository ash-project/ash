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

  def overview(apis) do
    """
    # API Overview

    #{for api <- apis, do: api_section(api)}
    """
  end

  def api_section(api) do
    """
    ## #{module_name(api)}

    ### Class Diagram

    ```mermaid
    #{Ash.Api.Info.Diagram.mermaid_class_diagram(api) |> String.trim()}
    ```

    ### ER Diagram

    ```mermaid
    #{Ash.Api.Info.Diagram.mermaid_er_diagram(api) |> String.trim()}
    ```

    ### Resources

    #{for resource <- Ash.Api.Info.resources(api), do: resource_section(resource)}
    """
  end

  def resource_section(resource) do
    """
    #### #{resource_name(resource)}

    #{Ash.Resource.Info.description(resource)}

    ##### Attributes

    | Name | Type | Description |
    | ---- | ---- | ----------- |
    #{for attr <- Ash.Resource.Info.attributes(resource), do: attr_section(attr)}
    """
  end

  def attr_section(attr) do
    """
    | **#{attr.name}** | #{short_module(attr.type)} | #{attr.description} |
    """
  end
end
