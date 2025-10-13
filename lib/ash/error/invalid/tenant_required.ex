# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.TenantRequired do
  @moduledoc "Used when a tenant is not specified"

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    "Queries against the #{inspect(resource)} resource require a tenant to be specified"
  end
end
