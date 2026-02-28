defmodule Ash.Notifier.PubSub.Verifiers.VerifyCalcTransforms do
  @moduledoc "Verifies that atom transforms reference valid calculations and derives `returns` from them."
  use Spark.Dsl.Verifier

  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    for publication <- Ash.Notifier.PubSub.Info.publications(dsl),
        is_atom(publication.transform) and not is_nil(publication.transform) do
      calc_name = publication.transform

      case Ash.Resource.Info.calculation(dsl, calc_name) do
        nil ->
          raise Spark.Error.DslError,
            path: [:pub_sub, :publish, publication.action || publication.type],
            module: module,
            message: """
            Publication references calculation `#{calc_name}` as transform, \
            but no such calculation exists on `#{inspect(module)}`.
            """

        %{type: :auto} ->
          raise Spark.Error.DslError,
            path: [:pub_sub, :publish, publication.action || publication.type],
            module: module,
            message: """
            Publication references calculation `#{calc_name}` as transform, \
            but that calculation has type `:auto` which could not be resolved. \
            Please set an explicit type on the calculation.
            """

        _calc ->
          :ok
      end
    end

    :ok
  end
end
