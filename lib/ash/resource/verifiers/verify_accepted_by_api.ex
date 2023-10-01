defmodule Ash.Resource.Verifiers.VerifyAcceptedByApi do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(dsl) do
    api = Spark.Dsl.Transformer.get_persisted(dsl, :api)

    if api do
      resource = Spark.Dsl.Verifier.get_persisted(dsl, :module)

      case Ash.Api.Info.resource(api, resource) do
        {:ok, _} ->
          :ok

        _ ->
          raise """
          Resource #{inspect(resource)} declared that its api is #{inspect(api)}, but that
          api does not accept this resource.

          The most likely cause for this is missing a call to `resource #{inspect(resource)}`
          in the `resources` block of #{inspect(api)}.
          """
      end
    else
      :ok
    end
  end
end
