# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Query.NoSuchFilterPredicate do
  @moduledoc "Used when a filter predicate that does not exist is referenced"

  use Splode.Error, fields: [:message, :value, :key, :resource], class: :invalid

  def message(%{key: key, resource: resource}) do
    "No such filter predicate for #{inspect(resource)}: #{inspect(key)}"
  end
end
