# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.PostComment do
  @moduledoc """
  Test resource for post comments.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :content, :string, allow_nil?: false, public?: true
    attribute :approved, :boolean, default: false, public?: true
  end

  relationships do
    belongs_to :post, Ash.Test.Manifest.Post, public?: true
    belongs_to :author, Ash.Test.Manifest.User, public?: true
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end
end
