# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.Build do
  @moduledoc false

  use Ash.Resource.Preparation

  def prepare(query, opts, _context) do
    Ash.Query.build(query, opts[:options] || [])
  end
end
