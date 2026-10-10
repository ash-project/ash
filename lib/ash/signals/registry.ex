# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Registry do
  @moduledoc """
  Finds the listeners of a signal module.

  This works like protocol consolidation. A resource that listens to signals defines a module
  named `Ash.Signals.Listeners.<Resource>`, which returns its listeners. Listeners are found by
  looking for modules with that prefix, among the loaded modules and the `.beam` files on the
  code path. Neither the signal module nor any domain needs to know about them.

  The `:ash_signals` compiler does this once per build, and compiles the result into an
  `Ash.Signals.Dispatch.<SignalModule>` module, so emitting a signal does not search for
  listeners. When there is no dispatch module, because the compiler is not used or
  `ash: [consolidate_signals: false]` is set in the project, listeners are searched for each
  time a signal is emitted. See `Mix.Tasks.Compile.AshSignals`.
  """

  @doc "Returns the listeners of the given signal, in a deterministic order."
  @spec listeners(Ash.Signals.t(), atom()) :: [Ash.Resource.SignalListener.t()]
  def listeners(signal_module, signal) do
    dispatch = dispatch_module(signal_module)

    if Code.ensure_loaded?(dispatch) do
      dispatch.listeners(signal)
    else
      signal_module
      |> discover()
      |> Enum.filter(&(&1.signal == signal))
    end
  end

  @doc "Finds all listeners of the given signal module, ignoring any dispatch module."
  @spec discover(Ash.Signals.t()) :: [Ash.Resource.SignalListener.t()]
  def discover(signal_module) do
    Enum.filter(discover_all(), &(&1.signal_module == signal_module))
  end

  @doc "Finds all listeners of all signal modules, ignoring any dispatch modules."
  @spec discover_all() :: [Ash.Resource.SignalListener.t()]
  def discover_all do
    Ash.Signals.Listeners
    |> modules_in_namespace()
    |> Enum.filter(&(Code.ensure_loaded?(&1) && function_exported?(&1, :listeners, 0)))
    |> Enum.flat_map(& &1.listeners())
  end

  @doc """
  Finds all signal modules, whether or not anything listens to them.

  Each signal module defines an empty `Ash.Signals.Index.<SignalModule>` module for this purpose.
  """
  @spec signal_modules() :: [Ash.Signals.t()]
  def signal_modules do
    Ash.Signals.Index
    |> modules_in_namespace()
    |> Enum.map(&strip_namespace(&1, Ash.Signals.Index))
    |> Enum.filter(&(Code.ensure_loaded?(&1) && Spark.Dsl.is?(&1, Ash.Signals)))
  end

  # Loaded modules and `.beam` files on the code path whose names start with `namespace.`,
  # sorted.
  defp modules_in_namespace(namespace) do
    prefix = Atom.to_string(namespace) <> "."

    on_disk =
      for dir <- :code.get_path(),
          {:ok, files} <- [:erl_prim_loader.list_dir(dir)],
          file <- files,
          file = List.to_string(file),
          String.starts_with?(file, prefix),
          String.ends_with?(file, ".beam") do
        file |> String.trim_trailing(".beam") |> String.to_atom()
      end

    loaded =
      for {module, _} <- :code.all_loaded(),
          String.starts_with?(Atom.to_string(module), prefix) do
        module
      end

    (on_disk ++ loaded)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp strip_namespace(module, namespace) do
    module
    |> Module.split()
    |> Enum.drop(length(Module.split(namespace)))
    |> Module.concat()
  end

  @doc false
  def index_module(signal_module), do: Module.concat(Ash.Signals.Index, signal_module)

  @doc false
  def dispatch_module(signal_module), do: Module.concat(Ash.Signals.Dispatch, signal_module)

  @doc false
  def listener_module(resource), do: Module.concat(Ash.Signals.Listeners, resource)
end
