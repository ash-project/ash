# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Domain do
  @moduledoc """
  Test domain for Ash.Info.Manifest integration testing.
  """
  use Ash.Domain,
    otp_app: :ash_manifest_test

  resources do
    resource Ash.Test.Manifest.Todo
    resource Ash.Test.Manifest.TodoComment
    resource Ash.Test.Manifest.User
    resource Ash.Test.Manifest.UserSettings
    resource Ash.Test.Manifest.OrgTodo
    resource Ash.Test.Manifest.Task
    resource Ash.Test.Manifest.NotExposed
    resource Ash.Test.Manifest.Post
    resource Ash.Test.Manifest.PostComment
    resource Ash.Test.Manifest.NoRelationshipsResource
    resource Ash.Test.Manifest.EmptyResource
    resource Ash.Test.Manifest.MapFieldResource
    resource Ash.Test.Manifest.Content
    resource Ash.Test.Manifest.Article
    resource Ash.Test.Manifest.Subscription
    resource Ash.Test.Manifest.TenantSetting
    resource Ash.Test.Manifest.InputParsing.Resource
    resource Ash.Test.Manifest.NestedMapResource
  end
end
