# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Calculation.RuntimeExpression do
  @moduledoc false
  use Ash.Resource.Calculation

  def calculate(records, opts, context) do
    Ash.Resource.Calculation.Expression.calculate(records, opts, context)
  end
end
