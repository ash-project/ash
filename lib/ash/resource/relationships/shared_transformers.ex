# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Relationships.SharedTransformers do
  @moduledoc false

  def manual_implies_no_attributes(relationship) do
    relationship
    |> Map.put(:no_attributes?, relationship.no_attributes? || not is_nil(relationship.manual))
  end
end
