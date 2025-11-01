# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

import Config

if Mix.env() == :dev do
  config :git_ops,
    mix_project: Ash.MixProject,
    github_handle_lookup?: true,
    changelog_file: "CHANGELOG.md",
    repository_url: "https://github.com/ash-project/ash",
    # Instructs the tool to manage your mix version in your `mix.exs` file
    # See below for more information
    manage_mix_version?: true,
    # Instructs the tool to manage the version in your README.md
    # Pass in `true` to use `"README.md"` or a string to customize
    manage_readme_version: [
      "README.md",
      "documentation/tutorials/get-started.md"
    ],
    version_tag_prefix: "v"
end

config :spark, :skip_diagnostic_warnings, true

config :logger, level: :warning

config :ash, :default_page_type, :keyset

config :ash, :ash_domains, [
  Ash.Test.Flow.Domain,
  Ash.Test.Support.PolicyRbac.Domain,
  Ash.Test.Support.PolicyComplex.Domain,
  Ash.Test.Support.PolicySimple.Domain
]

config :ash, :show_keysets_for_all_actions?, false
config :ash, :policies, no_filter_static_forbidden_reads?: false

config :ash, :custom_expressions, [Ash.Test.Expressions.JaroDistance]

config :ash, :keep_read_action_loads_when_loading?, false
config :ash, :read_action_after_action_hooks_in_order?, true
config :ash, :bulk_actions_default_to_errors?, true

config :crux, :sat_testing, true
config :ash, :no_join_mnesia_ets, :dynamic

config :ash, :validate_domain_resource_inclusion?, false
config :ash, :validate_domain_config_inclusion?, false
config :ash, :policies, show_policy_breakdowns?: true

config :ash, :compatible_foreign_key_types, [
  {Ash.Type.CiString, Ash.Type.UUID}
]

if config_env() == :test do
  config :elixir, :time_zone_database, Tz.TimeZoneDatabase

  config :ash, Ash.Type.UUIDv7, match_v4_uuids?: true
end
