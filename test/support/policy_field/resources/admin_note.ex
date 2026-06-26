# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.AdminNote do
  @moduledoc false
  use Ash.Resource, data_layer: :embedded

  attributes do
    attribute :body, :string do
      public?(true)
    end
  end
end
