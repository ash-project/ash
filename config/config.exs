import Config

if Mix.env() == :dev do
  config :git_ops,
    mix_project: Ash.MixProject,
    changelog_file: "CHANGELOG.md",
    repository_url: "https://github.com/ash-project/ash",
    # Instructs the tool to manage your mix version in your `mix.exs` file
    # See below for more information
    manage_mix_version?: true,
    # Instructs the tool to manage the version in your README.md
    # Pass in `true` to use `"README.md"` or a string to customize
    manage_readme_version: ["README.md", "documentation/tutorials/get-started.md"],
    version_tag_prefix: "v"
end

if Mix.env() == :test do
  config :ash, :ash_domains, [
    Ash.Test.Flow.Domain,
    Ash.Test.Support.PolicyRbac.Domain,
    Ash.Test.Support.PolicyComplex.Domain,
    Ash.Test.Support.PolicySimple.Domain
  ]

  config :ash, :sat_testing, true

  config :ash, :validate_domain_resource_inclusion?, false
  config :ash, :validate_domain_config_inclusion?, false
end
