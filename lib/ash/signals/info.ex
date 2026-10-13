# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Info do
  @moduledoc "Introspection for `Ash.Signals` modules."

  alias Spark.Dsl.Extension

  @doc "Returns all signals declared in the module."
  @spec signals(Spark.Dsl.t() | Ash.Signals.t()) :: [Ash.Signals.Signal.t()]
  def signals(module) do
    Extension.get_entities(module, [:signals])
  end

  @doc "Returns the signal with the given name, or `nil`."
  @spec signal(Spark.Dsl.t() | Ash.Signals.t(), atom()) :: Ash.Signals.Signal.t() | nil
  def signal(module, name) do
    module
    |> signals()
    |> Enum.find(&(&1.name == name))
  end
end
