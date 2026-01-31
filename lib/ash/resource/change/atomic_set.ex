# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.AtomicSet do
  @moduledoc """
  A change that sets an attribute using an expression evaluated in the data layer during create.

  This is used by the `atomic_set/3` builtin change. Unlike `Ash.Resource.Change.Atomic`
  (used by `atomic_update/3`), this is for setting values during the INSERT phase of create
  actions, not for updating existing values.

  The expression cannot reference existing database values via `atomic_ref/1` since there
  is no existing row during creates.

  When used on update actions, behaves the same as `atomic_update/3`.

  ## Return Value

  The `atomic/3` callback returns `{:atomic_set, atomics}` which tells Ash to apply
  these values during the INSERT phase (stored in `changeset.create_atomics`).
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
