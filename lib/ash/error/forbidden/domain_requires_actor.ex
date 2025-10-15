# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Forbidden.DomainRequiresActor do
  @moduledoc "Used when a domain that has `require_actor? true` is provided no actor"

  use Splode.Error, fields: [:domain], class: :forbidden

  def message(%{domain: domain}) do
    "The domain #{inspect(domain)} requires that an actor is provided at all times and none was provided."
  end
end
