defmodule Ash.Resource.Transformers.RequireUniqueActionNames do
  @moduledoc """
  Ensures that all actions have unique names.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  def transform(resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, actions} ->
      unless Enum.count(actions) == 1 do
        raise DslError.exception(
                module: resource,
                message: """
                Multiple actions (#{Enum.count(actions)}) with the name `#{name}` defined in #{inspect(resource)}.
                Actions: `#{inspect(actions)}`.

                In the past, the pattern of having multiple actions called `:default`
                was promoted in the documentation, but that is no longer valid. All
                actions must have unique names.
                """,
                path: [:actions, name]
              )
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.ValidatePrimaryActions), do: true
  def after?(_), do: false
end
