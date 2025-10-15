# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier.PubSub.Verifiers.VerifyActionNames do
  @moduledoc "Verifies action names in configured publications"
  use Spark.Dsl.Verifier

  def verify(dsl) do
    module =
      Spark.Dsl.Verifier.get_persisted(dsl, :module)

    for publication <- Ash.Notifier.PubSub.Info.publications(dsl),
        not is_nil(publication.action) do
      action = Ash.Resource.Info.action(dsl, publication.action)

      if !action do
        raise Spark.Error.DslError,
          path: [:pub_sub, :publish, publication.action],
          module: module,
          message: """
          Non-existent action #{inspect(module)}.#{publication.action} referenced in `pub_sub` notifier.
          """
      end

      if action.type == :action do
        raise Spark.Error.DslError,
          path: [:pub_sub, :publish, publication.action],
          module: module,
          message: """
          Invalid action #{inspect(module)}.#{publication.action} referenced in `pub_sub` notifier.

          Only create, update and destroy actions emit notifications, not generic actions.
          """
      end
    end

    :ok
  end
end
