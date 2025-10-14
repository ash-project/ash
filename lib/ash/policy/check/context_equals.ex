# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ContextEquals do
  @moduledoc "This check is true when the value of the specified key or path in the changeset or query context equals the specified value."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    case opts[:key] do
      key when is_atom(key) ->
        "query_or_changeset.context[#{inspect(key)}] == #{inspect(opts[:value])}"

      key when is_list(key) ->
        key =
          Enum.map_join(key, fn key ->
            "[#{inspect(key)}]"
          end)

        "query_or_changeset#{key} == #{inspect(opts[:value])}"
    end
  end

  @impl true
  def match?(_, context, opts) do
    get_in(context.subject.context || %{}, List.wrap(opts[:key])) == opts[:value]
  end
end
