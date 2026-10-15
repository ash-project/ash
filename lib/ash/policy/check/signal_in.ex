# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.SignalIn do
  @moduledoc "This check is true when the current action is being run by a listener of a signal."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "listening to #{inspect(options[:signal_module])}.#{options[:signal]}"
  end

  @impl true
  def match?(_actor, %{subject: %{context: context}}, options) do
    case context[:signal_path] do
      [%{signal_module: signal_module, signal: signal} | _] ->
        signal_module == options[:signal_module] && signal == options[:signal]

      _ ->
        false
    end
  end

  def match?(_, _, _), do: false
end
