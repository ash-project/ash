# SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Splode.Error.Unknown do
  @moduledoc "The default top level unknown error container"
  use Splode.ErrorClass, class: :unknown

  @impl true
  def exception(opts) do
    if opts[:error] do
      super(Keyword.update(opts, :errors, [opts[:error]], &[opts[:error] | &1]))
    else
      super(opts)
    end
  end
end
