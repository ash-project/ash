defmodule Ash.DataLayer.Simple.Transformers.ValidateDslSections do
  @moduledoc "Validates that a resource using the `simple` data layer has no relationships or aggregates"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  def transform(_resource, dsl_state) do
    with {:relationships, []} <-
           {:relationships, Transformer.get_entities(dsl_state, [:relationships])},
         {:aggregates, []} <- {:aggregates, Transformer.get_entities(dsl_state, [:aggregates])} do
      {:ok, dsl_state}
    else
      {type, [%{name: name} | _]} ->
        {:error,
         DslError.exception(
           module: __MODULE__,
           message:
             "Embedded resources or resources without a data layer cannot have any #{to_string(type)}.",
           path: [type, name]
         )}
    end
  end

  def after?(_), do: true
end
