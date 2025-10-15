# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualUpdate.Function do
  @moduledoc false
  use Ash.Resource.ManualUpdate

  def update(changeset, [fun: {m, f, a}], context) do
    apply(m, f, [changeset, context | a])
  end

  def update(changeset, [fun: fun], context) do
    fun.(changeset, context)
  end
end
