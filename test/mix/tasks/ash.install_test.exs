# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.InstallTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "writes the backwards compatibility config with an upcoming-defaults comment" do
    config =
      test_project()
      |> Igniter.compose_task("ash.install", [])
      |> apply_igniter!()
      |> Map.fetch!(:rewrite)
      |> Rewrite.source!("config/config.exs")
      |> Rewrite.Source.get(:content)

    # The comment explaining these are upcoming defaults sits directly above the block
    assert config =~
             """
             # These enable behaviors that will become the default in the next major
             # version of Ash. Setting them now opts your application into the new
             # behavior and ensures a seamless upgrade. See the backwards compatibility
             # guide for an explanation of each setting:
             # https://hexdocs.pm/ash/backwards-compatibility-config.html
             config :ash,
               allow_forbidden_field_for_relationships_by_default?: true,
               include_embedded_source_by_default?: false,
               show_keysets_for_all_actions?: false,
               default_page_type: :keyset,
               policies: [no_filter_static_forbidden_reads?: false],
               keep_read_action_loads_when_loading?: false,
               default_actions_require_atomic?: true,
               read_action_after_action_hooks_in_order?: true,
               bulk_actions_default_to_errors?: true,
               transaction_rollback_on_error?: true,
               redact_sensitive_values_in_errors?: true\
             """
  end
end
