defmodule Ash.Resource.Verifiers.VerifyNotifiers do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(dsl) do
    notifiers = Ash.Resource.Info.notifiers(dsl)

    case Enum.find(notifiers, fn notifier ->
           !Spark.implements_behaviour?(notifier, Ash.Notifier)
         end) do
      nil ->
        :ok

      notifier ->
        raise Spark.Error.DslError,
          module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
          message:
            "All notifiers must implement the Ash.Notifier behaviour. #{inspect(notifier)} did not. Either you are using an incorrect module, or you are missing `use Ash.Notifier` at the top.",
          path: [:notifiers]
    end

    :ok
  end
end
