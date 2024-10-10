cond do
  Application.compile_env(:ash, :sat_testing) ->
    defmodule Ash.SatSolver.Implementation do
      @moduledoc false
      def solve_expression(cnf) do
        Module.concat([System.get_env("SAT_SOLVER") || "Picosat"]).solve(cnf)
      end

      def check!, do: :ok
    end

  Code.ensure_loaded?(Picosat) ->
    defmodule Ash.SatSolver.Implementation do
      @moduledoc false
      def solve_expression(cnf) do
        Picosat.solve(cnf)
      end

      def check!, do: :ok
    end

  Code.ensure_loaded?(SimpleSat) ->
    defmodule Ash.SatSolver.Implementation do
      @moduledoc false
      def solve_expression(cnf) do
        SimpleSat.solve(cnf)
      end

      def check!, do: :ok
    end

  true ->
    defmodule Ash.SatSolver.Implementation do
      @moduledoc false
      def solve_expression(_cnf) do
        check!()

        :ok
      end

      def check! do
        if Code.ensure_loaded?(Picosat) || Code.ensure_loaded?(SimpleSat) do
          raise """
          No SAT solver available, although one was loaded.

          This typically means that you need to run `mix deps.compile ash --force`

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
