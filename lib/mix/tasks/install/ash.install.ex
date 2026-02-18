# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Mix.Tasks.Ash.Install do
    @shortdoc "Installs Ash into a project. Should be called with `mix igniter.install ash`"

    @moduledoc """
    #{@shortdoc}

    ## Options

    - `--example` - Creates some example resources. When used, will pass
      through options to `mix ash.gen.resource`. See that task docs for more.
    - `--setup` - Runs `mix ash.setup` after installation to set up extensions.
    - `--port` - PostgreSQL port for the install-time check (e.g. `--port 5433`).
    - `--host` - PostgreSQL host for the install-time check (e.g. `--host db.example.com`).
      Use with `--port` for script/CI runs without prompts.
    - `--yes` / `-y` - Assume yes for the "Continue anyway?" prompt when Postgres is not running.
    """

    use Igniter.Mix.Task

    # I know for a fact that this will spark lots of conversation, debate and bike shedding.
    # I will direct everyone who wants to debate about it here, and that will be all.
    #
    # Number of people who wanted this to be different: 2
    @resource_default_section_order [
      :resource,
      :code_interface,
      :actions,
      :policies,
      :pub_sub,
      :preparations,
      :changes,
      :validations,
      :multitenancy,
      :attributes,
      :relationships,
      :calculations,
      :aggregates,
      :identities
    ]

    @domain_default_section_order [
      :resources,
      :policies,
      :authorization,
      :domain,
      :execution
    ]

    @manual_lead_in """
    This guide will walk you through the process of manually installing Ash into your project.
    If you are starting from scratch, you can use `mix new` or `mix igniter.new` and follow these instructions.
    These installation instructions apply both to new projects and existing ones.
    """

    @skip_protocol_consolidation """
    To avoid warnings about protocol consolidation when recompiling in dev, we
    set protocol consolidation to happen only in non-dev environments.
    """

    @dependency_setup """
    See the readmes for `spark` and `reactor` for more information on their installation.
    We've included their changes here for your convenience.
    """

    @setup_formatter """
    Configure the DSL auto-formatter. This tells the formatter to remove excess parentheses
    and how to sort sections in your Ash.Resource & Ash.Domain modules for consistency.
    """

    @setup_backwards_compatibility """
    Configure backwards compatibility settings. See the [backwards compatibility guide](https://hexdocs.pm/ash/backwards-compatibility-config.html)
    for an explanation of each of the configurations.
    """

    @register_domains """
    Ash relies on your application's `:ash_domains` configuration to find domain modules for
    mix tasks (diagrams, livebooks, policy charts) and to validate that domains/resources are
    registered at compile time.

    Add your domain modules to `config/config.exs`:

    ```elixir
    config :my_app, :ash_domains, [
      MyApp.Accounts,
      MyApp.Helpdesk
    ]
    ```

    For more detail, see the [Domains guide](/documentation/topics/resources/domains.md#application-configuration-ash_domains).
    """

    @impl Igniter.Mix.Task
    def info(_argv, _parent) do
      %Igniter.Mix.Task.Info{
        composes: ["spark.install", "ash.gen.resource"],
        schema: [
          setup: :boolean,
          example: :boolean,
          port: :integer,  # e.g. mix igniter.install ash --port 5433
          host: :string,   # e.g. mix igniter.install ash --host 192.168.1.1
          yes: :boolean   # skip "Continue anyway?" when Postgres not running (script/CI)
        ],
        aliases: [y: :yes]
      }
    end
#check configuration for postgres opts if they were provided, if they were pass them into the function
#unix socket
    defp postgres_running?(opts) do
      port = Keyword.get(opts, :port, 5432)
      host = Keyword.get(opts, :host, ~c"127.0.0.1")

      case Ash.Mix.Tasks.Install.PostgresCheck.connect(host, port, [], 2_000) do
        #if we can connect to the port, we know Postgres is running (2 seconds timeout)
        {:ok, socket} ->
          #close the socket and return true
          Ash.Mix.Tasks.Install.PostgresCheck.close(socket)
          true

        {:error, _} ->
          #if we can't connect to the port, we know Postgres is not running
          false
      end
    end

    @impl Igniter.Mix.Task
    def igniter(igniter) do
      #if the user provides --port and --host, use them, otherwise use the default
      #example: if someone runs "mix igniter.install ash --port 5433 --host db.example.com", our function parses the args and the postgres_opts will be [:port, 5433, :host, ~c"db.example.com"]
      #this is then passed to the postgres_running? function
      postgres_opts = []
      postgres_opts =
        if port = igniter.args.options[:port],
          do: Keyword.put(postgres_opts, :port, port),
          else: postgres_opts

      postgres_opts =
        if host = igniter.args.options[:host],
          do: Keyword.put(postgres_opts, :host, String.to_charlist(host)),
          else: postgres_opts

      unless postgres_running?(postgres_opts) do
        Mix.shell().error("""
        Postgres is not running. Please start Postgres and re-run the installer (or run mix setup).
        This avoids partial installs that leave assets unbuilt when setup fails on DB connection.
        """)

        continue =
          if igniter.args.options[:yes] do
            true
          else
            Mix.shell().yes?("Continue anyway? [y/N]")
          end

        unless continue do
          exit({:shutdown, 1})
        end
      end

      igniter
      |> Igniter.Scribe.start_document(
          "Manual Installation",
          @manual_lead_in,
          app_name: :my_app
        )
        |> Igniter.Scribe.section("Install & Setup Dependencies", @dependency_setup, fn igniter ->
          igniter
          |> Igniter.Scribe.patch(&Igniter.compose_task(&1, "spark.install"))
          |> Igniter.Scribe.patch(&Igniter.compose_task(&1, "reactor.install"))
        end)
        |> Igniter.Scribe.section(
          "Skip protocol consolidation",
          @skip_protocol_consolidation,
          fn igniter ->
            igniter
            |> Igniter.Project.MixProject.update(:project, [:consolidate_protocols], fn
              nil ->
                {:ok,
                {:code,
                  quote do
                    Mix.env() != :dev
                  end}}

              other ->
                {:ok, other}
            end)
          end
        )
        |> Igniter.Scribe.section("Setup The Formatter", @setup_formatter, fn igniter ->
          igniter
          |> Igniter.Scribe.patch(&Igniter.Project.Formatter.import_dep(&1, :ash))
          |> Igniter.Scribe.patch(fn igniter ->
            igniter
            |> Spark.Igniter.prepend_to_section_order(
              :"Ash.Resource",
              @resource_default_section_order
            )
            |> Spark.Igniter.prepend_to_section_order(
              :"Ash.Domain",
              @domain_default_section_order
            )
          end)
        end)
        |> Igniter.Scribe.section(
          "Configure Dev/Test environments",
          @setup_backwards_compatibility,
          fn igniter ->
            Igniter.Scribe.patch(igniter, fn igniter ->
              igniter
              |> Igniter.Project.Config.configure(
                "test.exs",
                :ash,
                [:policies, :show_policy_breakdowns?],
                true
              )
              |> Igniter.Project.Config.configure(
                "dev.exs",
                :ash,
                [:policies, :show_policy_breakdowns?],
                true
              )
            end)
          end
        )
        |> Igniter.Scribe.section(
          "Setup Backwards Compatibility Configurations",
          @setup_backwards_compatibility,
          fn igniter ->
            Igniter.Scribe.patch(igniter, fn igniter ->
              igniter
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:allow_forbidden_field_for_relationships_by_default?],
                true
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:include_embedded_source_by_default?],
                false
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:show_keysets_for_all_actions?],
                false
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:default_page_type],
                :keyset
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:policies, :no_filter_static_forbidden_reads?],
                false
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:keep_read_action_loads_when_loading?],
                false
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:default_actions_require_atomic?],
                true
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:read_action_after_action_hooks_in_order?],
                true
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:bulk_actions_default_to_errors?],
                true
              )
              |> Igniter.Project.Config.configure(
                "config.exs",
                :ash,
                [:transaction_rollback_on_error?],
                true
              )
            end)
          end
        )
        |> Igniter.Scribe.section("Register Your Domains", @register_domains, fn igniter ->
        igniter
      end)
      |> then(fn igniter ->
          if igniter.args.options[:example] do
            generate_example(igniter, igniter.args.argv_flags)
          else
            igniter
          end
        end)
        |> then(fn igniter ->
          if igniter.args.options[:setup] do
            Igniter.delay_task(igniter, "ash.setup")
          else
            igniter
          end
        end)
    end

    defp generate_example(igniter, argv) do
      domain_module_name = Igniter.Project.Module.module_name(igniter, "Support")
      ticket_resource = Igniter.Project.Module.module_name(igniter, "Support.Ticket")

      representative_resource =
        Igniter.Project.Module.module_name(igniter, "Support.Representative")

      ticket_status_module_name =
        Igniter.Project.Module.module_name(igniter, "Support.Ticket.Types.Status")

      igniter
      |> Igniter.compose_task("ash.gen.domain", [
        inspect(domain_module_name),
        "--ignore-if-exists"
      ])
      |> Igniter.compose_task("ash.gen.enum", [
        inspect(ticket_status_module_name),
        "open,closed",
        "--ignore-if-exists",
        "--short-name",
        "ticket_status"
      ])
      |> Igniter.compose_task(
        "ash.gen.resource",
        [
          inspect(ticket_resource),
          "--ignore-if-exists",
          "--domain",
          inspect(domain_module_name),
          "--default-actions",
          "read",
          "--uuid-primary-key",
          "id",
          "--attribute",
          "subject:string:required:public",
          "--relationship",
          "belongs_to:representative:#{inspect(representative_resource)}:public"
        ] ++ argv
      )
      |> Igniter.compose_task(
        "ash.gen.resource",
        [
          inspect(representative_resource),
          "--ignore-if-exists",
          "--domain",
          inspect(domain_module_name),
          "--default-actions",
          "read,create",
          "--uuid-primary-key",
          "id",
          "--attribute",
          "name:string:required:public",
          "--relationship",
          "has_many:tickets:#{inspect(ticket_resource)}:public"
        ] ++ argv
      )
      |> Ash.Resource.Igniter.add_new_attribute(ticket_resource, :status, """
      attribute :status, :ticket_status do
        default :open
        allow_nil? false
      end
      """)
      |> Ash.Resource.Igniter.add_new_action(ticket_resource, :open, """
      create :open do
        accept [:subject]
      end
      """)
      |> Ash.Resource.Igniter.add_new_action(ticket_resource, :close, """
      update :close do
        accept []

        validate attribute_does_not_equal(:status, :closed) do
          message "Ticket is already closed"
        end

        change set_attribute(:status, :closed)
      end
      """)
      |> Ash.Resource.Igniter.add_new_action(ticket_resource, :assign, """
      update :assign do
        accept [:representative_id]
      end
      """)
    end
  end
else
  defmodule Mix.Tasks.Ash.Install do
    @moduledoc "Installs Ash into a project. Should be called with `mix igniter.install ash`"

    @shortdoc @moduledoc

    use Mix.Task

    def run(_argv) do
      Mix.shell().error("""
      The task 'ash.install' requires igniter to be run.

      Please install igniter and try again.

      For more information, see: https://hexdocs.pm/igniter
      """)

      exit({:shutdown, 1})
    end
  end
end
