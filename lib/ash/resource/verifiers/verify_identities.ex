defmodule Ash.Resource.Verifiers.VerifyIdentityFields do
  @moduledoc """
  Raises an error on potentially incompatible identity attributes.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    identities = Ash.Resource.Info.identities(dsl)

    for identity <- identities do
      for key <- identity.keys do
        unless Ash.Resource.Info.attribute(dsl, key) || Ash.Resource.Info.calculation(dsl, key) do
          raise Spark.Error.DslError,
            module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
            message: "All identity keys must be attributes or calculations. Got: #{inspect(key)}",
            path: [:identities, identity.name]
        end
      end
    end

    :ok
  end
end
