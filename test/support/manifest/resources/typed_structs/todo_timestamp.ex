# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoTimestamp do
  @moduledoc """
  Test TypedStruct for todo timestamp data.
  """
  use Ash.TypedStruct

  typed_struct do
    field(:created_by, :string, allow_nil?: false, description: "Test")
    field(:created_at, :utc_datetime, allow_nil?: false, description: "Test")
    field(:updated_by, :string, description: "Test")
    field(:updated_at, :utc_datetime, description: "Test")
  end
end
