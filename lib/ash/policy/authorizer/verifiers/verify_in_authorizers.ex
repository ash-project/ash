defmodule Ash.Policy.Authorizer.Verifiers.VerifyInAuthorizers do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    if !Ash.Resource.Info.resource?(module) ||
         Ash.Policy.Authorizer in List.wrap(Ash.Resource.Info.authorizers(module)) do
      :ok
    else
      {:error,
       Spark.Error.DslError.exception(
         module: module,
         message: """
         Ash.Policy.Authorizer was found in extensions, but not in `authorizers`.
         Please add it to the `authorizers` list, *not* the `extensions` list,
         otherwise policies would not be applied to actions.

         For example:

         use Ash.Resource,
           authorizers: [Ash.Policy.Authorizer]
         """
       )}
    end
  end
end
