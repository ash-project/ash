# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualRelationship.Function do
  @moduledoc false

  def load(records, [fun: {m, f, a}], context) do
    apply(m, f, [records, context | a])
  end

  def load(records, [fun: fun], context) do
    fun.(records, context)
  end
end
