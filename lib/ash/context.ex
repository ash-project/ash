# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Context do
  @moduledoc """
  Functions for working with the context provided to various callbacks in Ash.
  """

  @type context_keyword_list :: [
          {:actor, Ash.Resource.t()},
          {:authorize?, boolean()},
          {:tracer, Ash.Tracer.t()},
          {:tenant, Ash.Resource.t()}
        ]

  @doc """
  Copies keys from the given context map into a keyword list. Does *not* copy the `:domain` key.

  Keys copied:

  * `:actor`
  * `:authorize?`
  * `:tracer`
  * `:tenant`
  * `context[:shared]` -> `:context`
  """
  @doc deprecated: "Use `Ash.Scope.to_opts/2` instead"
  @spec to_opts(Ash.Scope.t(), Keyword.t()) :: context_keyword_list()
  def to_opts(map, opts \\ []) when is_map(map) do
    Ash.Scope.to_opts(map, opts)
  end
end
