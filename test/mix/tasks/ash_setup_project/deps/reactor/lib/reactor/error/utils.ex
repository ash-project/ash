# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Utils do
  @moduledoc false

  @doc "Attempt to describe an error"
  @spec describe_error(any) :: String.t()
  def describe_error(error) when is_exception(error), do: Exception.message(error)

  def describe_error(error) when is_binary(error) do
    if String.printable?(error) do
      error
    else
      inspect_error(error)
    end
  end

  def describe_error(error), do: inspect_error(error)

  @doc "Helper function to provide suggestions in error messages"
  @spec did_you_mean?(any, Enumerable.t(any)) :: nil | String.t()
  def did_you_mean?(requested, possible) do
    best_match =
      possible
      |> Enum.map(&inspect/1)
      |> Enum.max_by(
        &String.jaro_distance(&1, inspect(requested)),
        &>=/2,
        fn ->
          nil
        end
      )

    if best_match do
      "Did you mean `#{inspect(best_match)}`?"
    end
  end

  defp inspect_error(error) do
    inspected =
      error
      |> inspect()
      |> String.trim()

    if inspected =~ ~r/[\r\n\v]/ do
      """
      ```
      #{inspected}
      ```
      """
    else
      "`#{inspected}`"
    end
  end
end
