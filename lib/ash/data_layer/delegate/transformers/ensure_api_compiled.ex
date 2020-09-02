defmodule Ash.DataLayer.Delegate.Transformers.EnsureApiCompiled do
  @moduledoc "Validates and caches the primary key of a resource"
  use Ash.Dsl.Transformer

  alias Ash.DataLayer.Delegate
  alias Ash.Resouce.Transformers

  def transform(resource, dsl_state) do
    resource
    |> Delegate.api()
    |> Code.ensure_compiled()

    {:ok, dsl_state}
  end

  def before?(Transformers.ValidateActionTypesSupported), do: true
  def before?(_), do: false
end
