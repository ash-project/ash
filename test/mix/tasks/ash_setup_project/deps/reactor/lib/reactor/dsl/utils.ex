# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Utils do
  @moduledoc false

  alias Spark.{Dsl.Verifier, Error.DslError}

  @doc false
  def assert_unique_step_names(step_names, dsl_state) do
    step_names
    |> Enum.frequencies()
    |> Enum.reject(&(elem(&1, 1) == 1))
    |> case do
      [] ->
        :ok

      [{name, freq}] ->
        {:error,
         DslError.exception(
           module: Verifier.get_persisted(dsl_state, :module),
           path: [:reactor],
           message: """
           Reactor contains duplicate steps:

           There are #{freq} steps named `#{inspect(name)}`. Step names must be unique.
           """
         )}

      names_and_frequencies ->
        message =
          names_and_frequencies
          |> Enum.map(fn {name, freq} ->
            "  - `#{inspect(name)}` is repeated #{freq} times."
          end)

        {:error,
         DslError.exception(
           module: Verifier.get_persisted(dsl_state, :module),
           path: [:reactor],
           message: """
           Reactor contains duplicate steps:

           All step names must be unique, however you have the following duplicated step names:

           #{message}
           """
         )}
    end
  end
end
