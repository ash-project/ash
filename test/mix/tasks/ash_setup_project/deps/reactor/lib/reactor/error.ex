# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error do
  @moduledoc """
  Uses `splode` to manage various classes of error.
  """

  use Splode,
    error_classes: [
      reactor: Reactor.Error.Internal,
      invalid: Reactor.Error.Invalid,
      unknown: Reactor.Error.Unknown,
      validation: Reactor.Error.Validation
    ],
    unknown_error: Reactor.Error.Unknown.UnknownError

  @doc "Convenience wrapper around `use Splode.Error`"
  @spec __using__(keyword) :: Macro.output()
  defmacro __using__(opts) do
    quote do
      use Splode.Error, unquote(opts)
      import Reactor.Error.Utils
      import Reactor.Utils
    end
  end

  @doc """
  Recursively searches through nested reactor errors for the first instance of
  the provided module.
  """
  @spec fetch_error(Exception.t(), module) :: {:ok, Exception.t()} | :error
  def fetch_error(error, module) when is_exception(error, module), do: {:ok, error}

  def fetch_error(error, module) when is_list(error.errors) do
    Enum.reduce_while(error.errors, :error, fn error, :error ->
      case fetch_error(error, module) do
        {:ok, error} -> {:halt, {:ok, error}}
        :error -> {:cont, :error}
      end
    end)
  end

  def fetch_error(error, module) when is_exception(error.error),
    do: fetch_error(error.error, module)

  def fetch_error(_error, _module), do: :error

  @doc """
  Recursively searches through nested reactor errors and returns any errors
  which match the provided module name.
  """
  @spec find_errors(Exception.t(), module) :: [Exception.t()]
  def find_errors(error, module) when is_exception(error, module), do: [error]

  def find_errors(error, module) when is_list(error.errors) do
    Enum.flat_map(error.errors, &find_errors(&1, module))
  end

  def find_errors(error, module) when is_exception(error.error),
    do: find_errors(error.error, module)

  def find_errors(_error, _module), do: []
end
