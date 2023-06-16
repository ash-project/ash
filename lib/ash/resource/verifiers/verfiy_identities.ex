defmodule Ash.Resource.Verifiers.VerifyIdentityFields do
  @moduledoc """
  Raises an error on potentially incompatible identity attributes.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    identities = Ash.Resource.Info.identities(dsl)

    identity_names = Enum.flat_map(identities, fn identity -> identity.keys end)

    relations =
      dsl
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(fn relation ->
        relation.type == :belongs_to and relation.name in identity_names
      end)

    Enum.each(relations, fn rel ->
      raise Spark.Error.DslError,
        module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
        message:
          "Argument #{inspect(rel.name)} should be renamed as #{inspect(rel.name)}_id in `identities` block.",
        path: [:identities, :identity, rel.name]
    end)
  end
end
