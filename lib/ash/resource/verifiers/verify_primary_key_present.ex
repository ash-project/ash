defmodule Ash.Resource.Verifiers.VerifyPrimaryKeyPresent do
  @moduledoc """
  Raises an error when a required primary key is missing
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  def verify(dsl) do
    cond do
      !Enum.empty?(Ash.Resource.Info.primary_key(dsl)) ->
        :ok

      Ash.Resource.Info.embedded?(dsl) ->
        :ok

      !Verifier.get_option(dsl, [:resource], :require_primary_key?, true) ->
        :ok

      Enum.all?(Ash.Resource.Info.actions(dsl), &(&1.type == :action)) &&
          Enum.empty?(Ash.Resource.Info.fields(dsl)) ->
        :ok

      true ->
        message = """
        Resources that have at least one non-generic action must have a primary key.

        use the following to disable this requirement, but understand that various
        features may not work, and this should only be done if you know what you are doing.

        resource do
          require_primary_key? false
        end
        """

        {:error,
         Spark.Error.DslError.exception(
           module: Verifier.get_persisted(dsl, :module),
           path: [:attributes],
           message: message
         )}
    end
  end
end
