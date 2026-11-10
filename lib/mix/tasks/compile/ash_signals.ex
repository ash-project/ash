# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Compile.AshSignals do
  @moduledoc """
  Consolidates `Ash.Signals` listeners, once all modules have been compiled.

  Add it to your `mix.exs`:

      compilers: [:ash_signals] ++ Mix.compilers()

  If you use the Phoenix code reloader, add it to your endpoint's reloadable compilers too, so
  that listeners added in development are picked up:

      reloadable_compilers: [:ash_signals, :phoenix_live_view, :gettext, :elixir, :app]

  ## Consolidation

  Like protocol consolidation, this finds the listeners of each signal module once per build,
  and compiles them into an `Ash.Signals.Dispatch.<SignalModule>` module in your application's
  build directory, so that emitting a signal does not have to search for listeners.

  Listeners defined after compilation, like resources defined inside tests, are not part of the
  consolidated modules. To use them, disable consolidation in your project:

      def project do
        [
          ...,
          ash: [consolidate_signals: Mix.env() != :test]
        ]
      end

  Without consolidation, listeners are searched for every time a signal is emitted.
  """
  use Mix.Task.Compiler

  @recursive true

  @impl true
  def run(_args) do
    Mix.Task.Compiler.after_compiler(:elixir, fn
      {:noop, diagnostics} ->
        {:noop, diagnostics}

      {status, diagnostics} ->
        path = Mix.Project.compile_path()

        if Keyword.get(Mix.Project.config()[:ash] || [], :consolidate_signals, true) do
          sweep(path, consolidate(Ash.Signals.Registry.signal_modules(), path))
        else
          sweep(path, [])
        end

        {status, diagnostics}
    end)

    :noop
  end

  @impl true
  def clean do
    sweep(Mix.Project.compile_path(), [])
  end

  @doc false
  # Compiles and loads a dispatch module for each signal module, writing it to `path`, and
  # returns the dispatch modules.
  @spec consolidate([Ash.Signals.t()], Path.t()) :: [module]
  def consolidate(signal_modules, path) do
    listeners = Enum.group_by(Ash.Signals.Registry.discover_all(), & &1.signal_module)

    Enum.map(signal_modules, fn signal_module ->
      dispatch = Ash.Signals.Registry.dispatch_module(signal_module)
      clauses = listener_clauses(Map.get(listeners, signal_module, []))

      body =
        quote do
          @moduledoc false

          @doc false
          unquote_splicing(clauses)
          def listeners(_signal), do: []
        end

      # removing the old beam too, or Elixir loads it again to warn that it is being redefined
      unload(dispatch)
      File.rm(beam_path(path, dispatch))

      {:module, ^dispatch, binary, _} =
        Module.create(dispatch, body, Macro.Env.location(__ENV__))

      File.mkdir_p!(path)
      File.write!(beam_path(path, dispatch), binary)

      dispatch
    end)
  end

  defp listener_clauses(listeners) do
    listeners
    |> Enum.group_by(& &1.signal)
    |> Enum.sort()
    |> Enum.map(fn {signal, listeners} ->
      quote do
        def listeners(unquote(signal)), do: unquote(Macro.escape(listeners))
      end
    end)
  end

  # Removes dispatch modules in `path` other than the ones just written. Elixir's compiler
  # removes the modules defined while compiling a file when they are no longer defined, but
  # dispatch modules are written after it, so it does not know about them. Without this they
  # would be left behind when a signal module is removed, or when consolidation is disabled.
  defp sweep(path, keep) do
    prefix = "#{Ash.Signals.Dispatch}."

    with {:ok, files} <- File.ls(path) do
      for file <- files,
          String.starts_with?(file, prefix),
          String.ends_with?(file, ".beam"),
          module = file |> String.trim_trailing(".beam") |> String.to_atom(),
          module not in keep do
        unload(module)
        File.rm(Path.join(path, file))
      end
    end

    :ok
  end

  defp beam_path(path, module), do: Path.join(path, "#{module}.beam")

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
    :code.purge(module)
  end
end
