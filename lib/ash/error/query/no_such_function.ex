# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.NoSuchFunction do
  @moduledoc "Used when an function that doesn't exist is used in a query"

  use Splode.Error,
    fields: [:function, :arity, :resource, :could_be_calculation?],
    class: :invalid

  def message(error) do
    if error.arity do
      "No such function #{error.function}/#{error.arity}"
      |> for_resource(error.resource)
    else
      "No such function #{error.function}"
      |> for_resource(error.resource)
    end
    |> add_could_be_calculation(error)
  end

  defp add_could_be_calculation(message, error) do
    if error.could_be_calculation? do
      message <>
        "\n\n There is a calculation with the same name on the resource, did you mean to pass a keyword list instead of an arguments list?"
    else
      error
    end
  end

  defp for_resource(message, nil), do: message
  defp for_resource(message, resource), do: message <> " for resource #{inspect(resource)}"
end
