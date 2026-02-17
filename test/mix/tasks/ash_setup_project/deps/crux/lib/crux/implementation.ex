# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

moduledoc = """
This module provides an interface to a SAT solver.

It tries to use the `Picosat` module if available, falling back to `SimpleSat` if not.

If neither is available, it raises an error when attempting to solve an expression.

You can also specify a custom SAT solver by setting the `SAT_SOLVER` environment variable
to the name of a module that implements the `solve/1` function.

Alternatively, you can enable SAT testing by setting the `:sat_testing` configuration
for the `:crux` application. This will allow you to specify a custom SAT solver via
the `SAT_SOLVER` environment variable.
"""

check_doc = """
Checks if a SAT solver implementation is available.
Raises an error with instructions if not.
"""

cond do
  Application.compile_env(:crux, :sat_testing) ->
    defmodule Crux.Implementation do
      @moduledoc moduledoc

      @doc false
      def solve_expression(cnf) do
        env_solver = Module.concat([System.get_env("SAT_SOLVER") || "Picosat"])
        solver = Process.get(__MODULE__, env_solver)

        solver.solve(cnf)
      end

      @doc check_doc
      def check!, do: :ok
    end

  Application.compile_env(:ash, :sat_testing) ->
    IO.warn(
      """
      The `:sat_testing` configuration for the `:ash` application is deprecated.
      Please use the `:sat_testing` configuration for the `:crux` application instead.
      """,
      __ENV__
    )

    defmodule Crux.Implementation do
      @moduledoc moduledoc

      @doc false
      def solve_expression(cnf) do
        Module.concat([System.get_env("SAT_SOLVER") || "Picosat"]).solve(cnf)
      end

      @doc check_doc
      def check!, do: :ok
    end

  Code.ensure_loaded?(Picosat) ->
    defmodule Crux.Implementation do
      @moduledoc moduledoc

      @doc false
      def solve_expression(cnf) do
        Picosat.solve(cnf)
      end

      @doc check_doc
      def check!, do: :ok
    end

  Code.ensure_loaded?(SimpleSat) ->
    defmodule Crux.Implementation do
      @moduledoc moduledoc
      @doc false
      def solve_expression(cnf) do
        SimpleSat.solve(cnf)
      end

      @doc check_doc
      def check!, do: :ok
    end

  true ->
    defmodule Crux.Implementation do
      @moduledoc moduledoc
      def solve_expression(_cnf) do
        check!()

        # make the type checker happy
        apply(__MODULE__, :ok, [])
      end

      def ok do
        :ok
      end

      @doc check_doc
      def check! do
        if Code.ensure_loaded?(Picosat) || Code.ensure_loaded?(SimpleSat) do
          raise """
          No SAT solver available, although one was loaded.

          This typically means that you need to run `mix deps.compile crux --force`

          If that doesn't work, please ensure that one of the following dependencies is present in your application to use SAT solver features:

          * `:picosat_elixir` (recommended) - A NIF wrapper around the PicoSAT SAT solver. Fast, production ready, battle tested.
          * `:simple_sat` - A pure Elixir SAT solver. Slower than PicoSAT, but no NIF dependency.
          """
        end

        raise """
        No SAT solver available.

        Please add one of the following dependencies to your application to use SAT solver features:

        * `:picosat_elixir` (recommended) - A NIF wrapper around the PicoSAT SAT solver. Fast, production ready, battle tested.
        * `:simple_sat` - A pure Elixir SAT solver. Slower than PicoSAT, but no NIF dependency.
        """
      end
    end
end
