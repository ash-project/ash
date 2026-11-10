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
               allow_forbidden_field_for_relationships_by_default: true,
               include_embedded_source_by_default?: false,
               show_keysets_for_all_actions?: false,
               default_page_type: :keyset,
               policies: [no_filter_static_forbidden_reads?: false],
               keep_read_action_loads_when_loading?: false,
               default_actions_require_atomic?: true,
               read_action_after_action_hooks_in_order?: true,
               bulk_actions_default_to_errors?: true,
               transaction_rollback_on_error?: true,
               redact_sensitive_values_in_errors?: true,
               default_string_length_count: :codepoints,
               infer_generic_action_reactors?: false\
             """
  end

  test "configures keyset signing secrets for each environment" do
    rewrite =
      test_project()
      |> Igniter.compose_task("ash.install", [])
      |> apply_igniter!()
      |> Map.fetch!(:rewrite)

    dev_secret = keyset_signing_secret(rewrite, "config/dev.exs")
    test_secret = keyset_signing_secret(rewrite, "config/test.exs")

    assert byte_size(Base.decode64!(dev_secret, padding: false)) == 32
    assert byte_size(Base.decode64!(test_secret, padding: false)) == 32
    refute dev_secret == test_secret

    runtime = config_content(rewrite, "config/runtime.exs")

    assert runtime =~ "if config_env() == :prod do"
    assert runtime =~ ~s|System.get_env("ASH_KEYSET_SIGNING_SECRET")|
    assert runtime =~ "environment variable ASH_KEYSET_SIGNING_SECRET is missing"
  end

  test "uses a placeholder keyset signing secret when generating docs" do
    rewrite =
      test_project()
      |> Igniter.assign(:scribe?, true)
      |> Igniter.compose_task("ash.install", [])
      |> apply_igniter!()
      |> Map.fetch!(:rewrite)

    assert keyset_signing_secret(rewrite, "config/dev.exs") == "<a long random string>"
    assert keyset_signing_secret(rewrite, "config/test.exs") == "<a long random string>"
  end

  test "derives the prod keyset signing secret from Phoenix's secret_key_base" do
    rewrite =
      test_project(
        files: %{
          "config/dev.exs" => """
          import Config

          config :test, TestWeb.Endpoint, secret_key_base: "phoenix dev secret"
          """
        }
      )
      |> Igniter.compose_task("ash.install", [])
      |> apply_igniter!()
      |> Map.fetch!(:rewrite)

    runtime = config_content(rewrite, "config/runtime.exs")
    refute runtime =~ "ASH_KEYSET_SIGNING_SECRET"

    previous = System.get_env("SECRET_KEY_BASE")
    System.put_env("SECRET_KEY_BASE", "phoenix prod secret")

    on_exit(fn ->
      if previous,
        do: System.put_env("SECRET_KEY_BASE", previous),
        else: System.delete_env("SECRET_KEY_BASE")
    end)

    config = Config.Reader.eval!("config/runtime.exs", runtime, env: :prod)

    assert config[:ash][:keyset_signing_secret] ==
             :crypto.mac(:hmac, :sha256, "phoenix prod secret", "ash keyset signing")
  end

  defp keyset_signing_secret(rewrite, path) do
    [_, secret] =
      Regex.run(~r/keyset_signing_secret: "([^"]+)"/, config_content(rewrite, path))

    secret
  end

  defp config_content(rewrite, path) do
    rewrite
    |> Rewrite.source!(path)
    |> Rewrite.Source.get(:content)
  end
end
