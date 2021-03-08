defmodule Ash.Api.Transformers.UniqueFunctionNames do
  @moduledoc """
  Ensures that all function names added to the API will be unique.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  require Logger

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(_module, dsl) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.flat_map(fn %{resource: resource} = reference ->
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.map(fn action ->
        action.as || :"#{Ash.Api.Interface.name(reference)}_#{action.name}"
      end)
    end)
    |> Enum.reduce(%{}, fn name, acc ->
      Map.update(acc, name, 0, &(&1 + 1))
    end)
    |> Enum.each(fn {name, count} ->
      if count > 1 do
        raise Ash.Error.Dsl.DslError.exception(
                module: __MODULE__,
                message: """
                Multiple actions would share the Api helper name #{name}.
                Specify the `as` option to update the helper name for those actions so they do not match.
                Alternatively, specify the `as` option for those resources so they do not have the same short name.
                """,
                path: [:actions]
              )
      end
    end)

    {:ok, dsl}
  end
end
