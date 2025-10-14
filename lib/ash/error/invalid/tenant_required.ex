# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Invalid.TenantRequired do
  @moduledoc "Used when a tenant is not specified"

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    "Queries against the #{inspect(resource)} resource require a tenant to be specified"
  end
end
