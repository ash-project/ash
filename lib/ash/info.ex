defmodule Ash.Info do
  @moduledoc """
  General introspection helpers for Ash applications.

  Use `Ash.Info.mermaid_overview(otp_app)` to generate a mermaid chart of the application's domains and resources.
  This offers a high level view, but will not include information from extensions. Use `resource_report/2`
  for a detailed report of a resource.

  Use `Ash.Info.mermaid_overview(otp_app, type: :entity_relationship)` to get a simplified entity relationship diagram.

  Use `Ash.Info.domains_and_resources(otp_app)` to get a map where the keys are the domains, and the value is the list
  of resources for that domain.
  """

  @spec domains(atom()) :: [Ash.Domain.t()]
  def domains(otp_app) do
    Application.get_env(otp_app, :ash_domains, [])
  end

  @doc """
  Returns a map where the keys are the domains, and the value is the list of resources for that domain.
  """
  @spec domains_and_resources(atom()) :: %{Ash.Domain.t() => [Ash.Resource.t()]}
  def domains_and_resources(otp_app) do
    otp_app
    |> Application.get_env(:ash_domains, [])
    |> Map.new(&{&1, Ash.Domain.Info.resources(&1)})
  end

  @doc """
  Generate a mermaid chart of the application's domains and resources.

  This offers a high level view, but will not include information from extensions.
  """
  def mermaid_overview(otp_app, type \\ :class) do
    case type do
      :class ->
        Ash.Domain.Info.Diagram.mermaid_class_diagram(domains(otp_app),
          show_private?: true,
          name: :full,
          include_source_links?: true
        )

      :entity_relationship ->
        Ash.Domain.Info.Diagram.mermaid_er_diagram(domains(otp_app),
          show_private?: true,
          name: :full,
          include_source_links?: true
        )
    end
  end
end
