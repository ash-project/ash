# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyExtensionsAreNotMisplaced do
  @moduledoc false
  use Spark.Dsl.Verifier

  @extension_kinds [
    {Ash.Authorizer, :authorizers, "authorizers"},
    {Ash.Notifier, :notifiers, "notifiers"}
  ]

  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)
    extensions = Spark.Dsl.Verifier.get_persisted(dsl, :extensions, [])

    for extension <- extensions,
        {behaviour, kind, kind_name} <- @extension_kinds,
        Spark.implements_behaviour?(extension, behaviour),
        registered = Spark.Dsl.Verifier.get_persisted(dsl, kind, []),
        extension not in registered do
      raise Spark.Error.DslError,
        module: module,
        path: [:extensions],
        message:
          "#{inspect(extension)} implements the #{inspect(behaviour)} behaviour " <>
            "but was added to `extensions` instead of `#{kind_name}`. " <>
            "The DSL will be available but it won't be used for #{kind_name}. " <>
            "Move it to the `#{kind_name}` option."
    end

    :ok
  end
end
