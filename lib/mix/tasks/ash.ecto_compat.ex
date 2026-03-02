# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Ash.EctoCompat do
  use Mix.Task

  @shortdoc "Check Ash resources for Ecto autogenerate/default compatibility issues"

  @moduledoc """
  Runs compatibility checks between Ash resources and their generated Ecto schemas.

  This task surfaces differences between how Ash handles timestamps and defaults
  versus how Ecto handles them. It is useful for developers who need to use both
  `Ash.create/2` and `Repo.insert/1` on the same resource, or who are migrating
  from Ecto schemas to Ash resources.

  See: https://github.com/ash-project/ash/issues/769

  ## Usage

  Check all resources discovered from configured domains:

      mix ash.ecto_compat

  Check a single resource by module name:

      mix ash.ecto_compat --resource MyApp.User

  Check all resources within a specific domain:

      mix ash.ecto_compat --domain MyApp.Domain

  ## Exit Codes

    * `0` — No compatibility warnings found (or no resources to check).
    * `1` — One or more compatibility warnings were found.
  """

  # Define the CLI switches we accept. Both are optional strings.
  @switches [
    resource: :string,
    domain: :string
  ]

  @impl true
  def run(args) do
    # Parse CLI arguments into keyword opts.
    # `_args` captures any positional args (we don't use them).
    # The trailing `_` captures any unknown switches (ignored).
    {opts, _args, _} = OptionParser.parse(args, switches: @switches)

    # Boot the application so all modules are compiled and loaded.
    # This is necessary because we need to call functions on resource
    # and domain modules, which must be available in the VM.
    Mix.Task.run("app.start")

    # Determine which resources to check based on CLI options.
    resources = get_resources(opts)

    if resources == [] do
      Mix.shell().info("No Ash resources found to check.")
      :ok
    else
      check_resources(resources)
    end
  end

  # Runs the compatibility checks against the given list of resources,
  # prints per-resource results, and outputs a summary.
  defp check_resources(resources) do

    Mix.shell().info("Checking #{length(resources)} resource(s) for Ecto compatibility issues...\n")

    # Run `Ash.EctoCompat.inspect_resource/1` on each resource and collect
    # all warnings into a flat list. We also tag each warning with
    # `:resource_module` so we can group them later for display.
    all_warnings =
      resources
      |> Enum.flat_map(fn resource ->
        case Ash.EctoCompat.inspect_resource(resource) do
          warnings when is_list(warnings) ->
            # Tag each warning with the resource module for grouping.
            Enum.map(warnings, &Map.put(&1, :resource_module, resource))

          error ->
            # Shouldn't happen since inspect_resource always returns a list,
            # but handle it defensively.
            [error]
        end
      end)

    # Group warnings by resource so we can print a clear per-resource report.
    warnings_by_resource =
      all_warnings
      |> Enum.group_by(& &1[:resource_module] || &1.resource)

    # Print the warnings for each resource using the formatter in Ash.EctoCompat.
    Enum.each(warnings_by_resource, fn {resource, warnings} ->
      if warnings != [] do
        Mix.shell().info("📦 #{inspect(resource)}")
        Ash.EctoCompat.print_warnings(warnings)
      else
        Mix.shell().info("✅ #{inspect(resource)} - No issues")
      end
    end)

    # Print a summary. We use Mix.shell().error for warnings so it stands
    # out in CI output, but we don't call System.halt — that would kill the
    # VM and break programmatic callers or test runners.
    total_warnings = length(all_warnings)

    if total_warnings > 0 do
      Mix.shell().error("\n⚠️  Found #{total_warnings} compatibility warning(s)")
    else
      Mix.shell().info("\n✅ All resources are Ecto-compatible!")
    end
  end

  # ── Resource discovery ─────────────────────────────────────────────────

  # Determines which resources to check based on CLI options.
  # Three modes:
  #   1. --resource MyApp.User  -> check that one resource
  #   2. --domain MyApp.Domain  -> check all resources in that domain
  #   3. (no flags)             -> auto-discover all resources from all
  #                                configured domains in the application
  defp get_resources(opts) do
    cond do
      resource = opts[:resource] ->
        # Mode 1: Check a specific resource module.
        # `Module.concat/1` converts the string "MyApp.User" into the atom
        # `MyApp.User`. This is safe because mix tasks run in a trusted context.
        try do
          resource_module = Module.concat([resource])
          [resource_module]
        rescue
          _ ->
            Mix.shell().error("Invalid resource name: #{resource}")
            []
        end

      domain = opts[:domain] ->
        # Mode 2: Check all resources belonging to a specific domain.
        # We load the domain module and call its `resources/0` function,
        # which is generated by the Ash.Domain DSL.
        try do
          domain_module = Module.concat([domain])

          if Code.ensure_loaded?(domain_module) do
            if function_exported?(domain_module, :resources, 0) do
              domain_module.resources()
            else
              Mix.shell().error("Domain #{inspect(domain_module)} doesn't export resources/0")
              []
            end
          else
            Mix.shell().error("Domain #{inspect(domain_module)} not found")
            []
          end
        rescue
          _ ->
            Mix.shell().error("Invalid domain name: #{domain}")
            []
        end

      true ->
        # Mode 3: Auto-discover all resources from all configured domains.
        find_all_resources()
    end
  end

  # Auto-discovers all Ash resources by:
  #   1. Finding all OTP apps in the project (supports umbrella apps).
  #   2. Reading the `:ash_domains` config key from each app's environment.
  #   3. Asking each domain for its list of resources.
  #
  # This mirrors how Ash itself discovers domains at runtime, using the
  # standard `config :my_app, ash_domains: [MyApp.Domain]` convention.
  defp find_all_resources do
    # Determine which OTP apps to scan. In an umbrella project,
    # `Mix.Project.apps_paths/0` returns a map of app names to paths.
    # In a regular project, we just use the current app.
    apps =
      if Code.ensure_loaded?(Mix.Project) do
        if apps_paths = Mix.Project.apps_paths() do
          apps_paths |> Map.keys() |> Enum.sort()
        else
          [Mix.Project.config()[:app]]
        end
      else
        []
      end

    # Collect all configured domains across all apps.
    # Users configure these with: `config :my_app, ash_domains: [MyApp.Domain]`
    domains =
      apps
      |> Enum.flat_map(&Application.get_env(&1, :ash_domains, []))
      |> Enum.uniq()

    if domains == [] do
      Mix.shell().info(
        "No domains found. Please use --resource or --domain to specify what to check, " <>
          "or configure domains in config with `config :my_app, ash_domains: [...]`"
      )

      []
    else
      # Ask each domain for its resources. Domains define a `resources/0`
      # function via the Ash.Domain DSL that returns all registered resources.
      resources =
        domains
        |> Enum.flat_map(fn domain ->
          if Code.ensure_loaded?(domain) and function_exported?(domain, :resources, 0) do
            domain.resources()
          else
            []
          end
        end)
        |> Enum.uniq()

      if resources == [] do
        Mix.shell().info("No resources found in configured domains.")
      end

      resources
    end
  end
end
