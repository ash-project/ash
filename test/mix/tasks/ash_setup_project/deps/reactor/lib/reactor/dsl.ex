# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl do
  @moduledoc false

  alias Reactor.Dsl
  alias Spark.Dsl.{Extension, Section}

  @middleware %Section{
    name: :middlewares,
    describe: "Middleware to be added to the Reactor",
    entities: [Dsl.Middleware.__entity__()],
    patchable?: true
  }

  @reactor %Section{
    name: :reactor,
    describe: "The top-level reactor DSL",
    schema: [
      return: [
        type: :atom,
        required: false,
        doc: """
        Specify which step result to return upon completion.
        """
      ],
      description: [
        type: :string,
        required: false,
        doc: """
        An optional description for the Reactor.
        """
      ]
    ],
    entities: [
      Dsl.Around.__entity__(),
      Dsl.Collect.__entity__(),
      Dsl.Compose.__entity__(),
      Dsl.Debug.__entity__(),
      Dsl.Flunk.__entity__(),
      Dsl.Group.__entity__(),
      Dsl.Input.__entity__(),
      Dsl.Map.__entity__(),
      Dsl.Recurse.__entity__(),
      Dsl.Step.__entity__(),
      Dsl.Switch.__entity__(),
      Dsl.Template.__entity__()
    ],
    sections: [@middleware],
    top_level?: true,
    patchable?: true
  }

  use Extension,
    sections: [@reactor],
    transformers: [Dsl.Transformer],
    verifiers: [Dsl.Verifier]
end
