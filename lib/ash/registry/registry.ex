defmodule Ash.Registry do
  @moduledoc """
  A registry allows you to separate your resources from your `api` module, to reduce improve compile times and reduce compile time dependencies.

  For example:

  ```elixir
  defmodule MyApp.MyRegistry do
    use Ash.Registry

    entries do
      entry MyApp.Resource
      entry MyApp.OtherResource
    end
  end
  ```
  """

  @type t :: module

  use Ash.Dsl, default_extensions: [extensions: [Ash.Registry.Dsl]]

  alias Ash.Dsl.Extension

  @spec entries(t()) :: list(module)
  def entries(registry) do
    case registry |> Extension.get_entities([:entries]) |> Enum.map(& &1.entry) do
      [] ->
        registry |> Extension.get_entities([:resources]) |> Enum.map(& &1.resource)

      other ->
        other
    end
  end

  @spec api_or_api_and_registry(Ash.Api.t() | {Ash.Api.t(), t()}) :: {t(), t()}
  def api_or_api_and_registry({api, registry}), do: {api, registry}
  def api_or_api_and_registry(api), do: {api, api}
end
