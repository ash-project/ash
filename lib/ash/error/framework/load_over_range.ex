# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Framework.LoadOverRange do
  @moduledoc "Used when a write over a range is asked to load calculations, aggregates or relationships"

  use Splode.Error, fields: [:resource, :as_of, :load], class: :framework

  def message(error) do
    """
    Cannot load #{inspect(error.load)} when writing #{inspect(error.resource)} over #{inspect(error.as_of)}.

    A write over a range spans every version it overlaps, so there is no single instant for
    calculations, aggregates or relationships (or `now()`) to be answered at. Load them with
    a separate read as of an instant instead.
    """
  end
end
