defmodule Ash.Resource.Verifiers.VerifyAcceptedByDomain do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(dsl) do
    domain = Spark.Dsl.Transformer.get_persisted(dsl, :domain)

    if domain && Code.ensure_loaded?(domain) do
      resource = Spark.Dsl.Verifier.get_persisted(dsl, :module)

      case Ash.Domain.Info.resource(domain, resource) do
        {:ok, _} ->
          :ok

        _ ->
          raise """
          Resource #{inspect(resource)} declared that its domain is #{inspect(domain)}, but that
          domain does not accept this resource.

          The most likely cause for this is missing a call to `resource #{inspect(resource)}`
          in the `resources` block of #{inspect(domain)}.
          """
      end
    else
      :ok
    end
  end
end
