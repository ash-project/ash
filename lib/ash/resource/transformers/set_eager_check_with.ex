defmodule Ash.Resource.Transformers.SetEagerCheckWith do
  @moduledoc false
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    domain = Transformer.get_persisted(dsl_state, :domain)

    dsl_state
    |> Ash.Resource.Info.identities()
    |> Enum.reject(&(&1.eager_check? == false && &1.eager_check_with == nil))
    |> Enum.reduce({:ok, dsl_state}, fn
      %{eager_check?: true, eager_check_with: nil} = identity, {:ok, dsl_state} ->
        unless domain do
          raise Spark.Error.DslError,
            module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
            path: [:identities, identity.name, :eager_check?],
            message: """
            Cannot infer eager_check_with, because the domain is not specified on this resource."
            """
        end

        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:identities],
            %{identity | eager_check_with: domain},
            &(&1.name == identity.name)
          )

        {:ok, new_dsl_state}

      %{eager_check?: false} = identity, {:ok, dsl_state} ->
        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:identities],
            %{identity | eager_check?: true},
            &(&1.name == identity.name)
          )

        {:ok, new_dsl_state}
    end)
  end
end
