# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.SetContext do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  def prepare(subject, opts, _context) do
    context =
      case opts[:context] do
        {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [subject | a])

        other ->
          {:ok, other}
      end

    case context do
      {:ok, context} ->
        Ash.Subject.set_context(subject, context)

      {:error, error} ->
        Ash.Subject.add_error(subject, error)

      context ->
        Ash.Subject.set_context(subject, context)
    end
  end
end
