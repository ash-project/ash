defmodule Ash.Resource.Transformers.SetPreCheckWith do
  @moduledoc false
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    domain = Transformer.get_persisted(dsl_state, :domain)

    dsl_state
    |> Ash.Resource.Info.identities()
    |> Enum.reject(&(&1.pre_check? == false && &1.pre_check_with == nil))
    |> Enum.reduce({:ok, dsl_state}, fn
      %{pre_check?: true, pre_check_with: nil} = identity, {:ok, dsl_state} ->
        unless domain do
          raise Spark.Error.DslError,
            module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
            path: [:identities, identity.name, :pre_check?],
            message: """
            Cannot infer pre_check_with, because the domain is not specified on this resource."
            """
        end

        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:identities],
            %{identity | pre_check_with: domain},
            &(&1.name == identity.name)
          )

        {:ok, new_dsl_state}

      %{pre_check?: false} = identity, {:ok, dsl_state} ->
        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:identities],
            %{identity | pre_check?: true},
            &(&1.name == identity.name)
          )

        {:ok, new_dsl_state}
    end)
  end
end
