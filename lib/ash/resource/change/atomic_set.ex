# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.AtomicSet do
  @moduledoc """
  A change that sets an attribute using an expression during the create phase.

  Unlike `Ash.Resource.Change.Atomic`, this is for setting values during INSERT,
  not for updating existing values. The expression cannot reference existing
  database values via `atomic_ref/1`.
  """
  use Ash.Resource.Change

  @impl true
  def change(changeset, opts, _) do
    if opts[:cast_atomic?] do
      Ash.Changeset.atomic_set(changeset, opts[:attribute], opts[:expr])
    else
      Ash.Changeset.atomic_set(changeset, opts[:attribute], {:atomic, opts[:expr]})
    end
  end

  @impl true
  def atomic(_changeset, opts, _context) do
    if opts[:cast_atomic?] do
      {:atomic_set, %{opts[:attribute] => opts[:expr]}}
    else
      {:atomic_set, %{opts[:attribute] => {:atomic, opts[:expr]}}}
    end
  end
end
