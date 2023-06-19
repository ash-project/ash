defmodule Ash.Resource.Verifiers.VerifyIdentityFields do
  @moduledoc """
  Raises an error on potentially incompatible identity attributes.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    identities = Ash.Resource.Info.identities(dsl)

    for identity <- identities do
      for key <- identity.keys do
        if !Ash.Resource.Info.attribute(dsl, key) do
          raise Spark.Error.DslError,
            module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
            message: "All identity keys must be attributes. Got: #{inspect(key)}",
            path: [:identities, identity.name]
        end
      end
    end

    :ok
  end
end
