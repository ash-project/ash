# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.Build do
  @moduledoc false

  use Ash.Resource.Preparation

  def prepare(query, opts, _context) do
    Ash.Query.build(query, opts[:options] || [])
  end
end
