# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.MixProject do
  use Mix.Project

  @version "1.0.0"
  @description "An asynchronous, graph-based execution engine"

  def project do
    [
      app: :reactor,
      version: @version,
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: @description,
      elixirc_paths: elixirc_paths(Mix.env()),
      aliases: aliases(),
      source_url: "https://github.com/ash-project/reactor",
      homepage_url: "https://github.com/ash-project/reactor",
      dialyzer: [plt_add_apps: [:mix]],
      docs: fn ->
        [
          main: "readme",
          logo: "logos/reactor-logo-light-small.png",
          extras: [
            "README.md",
            # Tutorials
            "documentation/tutorials/01-getting-started.md",
            "documentation/tutorials/02-error-handling.md",
            "documentation/tutorials/03-async-workflows.md",
            "documentation/tutorials/04-composition.md",
            "documentation/tutorials/05-recursive-execution.md",
            "documentation/tutorials/reactor-cheatsheet.cheatmd",
            # How-to Guides
            "documentation/how-to/payment-processing.md",
            "documentation/how-to/data-pipelines.md",
            "documentation/how-to/api-orchestration.md",
            "documentation/how-to/testing-strategies.md",
            "documentation/how-to/debugging-workflows.md",
            "documentation/how-to/performance-optimization.md",
            # Reference
            {"documentation/dsls/DSL-Reactor.md",
             search_data: Spark.Docs.search_data_for(Reactor.Dsl)},
            "documentation/reference/glossary.md",
            # Explanation
            "documentation/explanation/concepts.md",
            "documentation/explanation/architecture.md",
            "documentation/explanation/design-decisions.md",
            "documentation/explanation/ecosystem.md"
          ],
          groups_for_extras: extra_documentation_groups(),
          before_closing_head_tag: fn type ->
            if type == :html do
              """
              <script>
                if (location.hostname === "hexdocs.pm") {
                  var script = document.createElement("script");
                  script.src = "https://plausible.io/js/script.js";
                  script.setAttribute("defer", "defer")
                  script.setAttribute("data-domain", "ashhexdocs")
                  document.head.appendChild(script);
                }
              </script>
              """
            end
          end,
          before_closing_body_tag: fn type ->
            if type == :html do
              """
              <script src="https://cdn.jsdelivr.net/npm/mermaid@10.2.3/dist/mermaid.min.js"></script>
              <script>
                mermaid.initialize({
                  startOnLoad: true,
                  theme: 'default'
                });
              </script>
              """
            end
          end,
          groups_for_modules: [
            Dsl: ~r/^Reactor\.Dsl.*/,
            Steps: ~r/^Reactor\.Step.*/,
            Middleware: ~r/^Reactor\.Middleware.*/,
            Errors: ~r/^Reactor\.Error.*/,
            Builder: ~r/^Reactor\.Builder.*/,
            Internals: ~r/^Reactor\..*/
          ],
          extra_section: "GUIDES",
          formatters: ["html"],
          filter_modules: ~r/^Elixir.Reactor/,
          source_url_pattern: "https://github.com/ash-project/reactor/blob/main/%{path}/#L%{line}"
        ]
      end
    ]
  end

  defp package do
    [
      name: :reactor,
      files:
        ~w[lib .formatter.exs mix.exs README* LICENSE* CHANGELOG* documentation usage-rules.md],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/ash-project/reactor",
        "Changelog" => "https://github.com/ash-project/reactor/blob/main/CHANGELOG.md",
        "REUSE Compliance" => "https://api.reuse.software/info/github.com/ash-project/reactor"
      },
      maintainers: [
        "James Harton <james@harton.nz>",
        "Zach Daniel <zach@zachdaniel.dev>"
      ],
      source_url: "https://github.com/ash-project/reactor"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Reactor.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:spark, "~> 2.3 and >= 2.3.3"},
      {:splode, "~> 0.2"},
      {:libgraph, "~> 0.16"},
      {:igniter, "~> 0.4", optional: true},
      {:iterex, "~> 0.1"},
      {:jason, "~> 1.0"},
      {:telemetry, "~> 1.2"},
      {:yaml_elixir, "~> 2.11"},
      {:ymlr, "~> 5.0"},

      # Dev/Test dependencies
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:doctor, "~> 0.18", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.37-rc", only: [:dev, :test], runtime: false},
      {:ex_check, "~> 0.16.0", only: [:dev, :test]},
      {:git_ops, "~> 2.9.0", only: [:dev, :test]},
      {:mimic, "~> 2.0", only: :test},
      {:mix_audit, "~> 2.1", only: [:dev, :test], runtime: false},
      {:sobelow, ">= 0.0.0", only: [:dev, :test], runtime: false}
    ]
  end

  defp elixirc_paths(env) when env in ~w[dev test]a do
    elixirc_paths(:prod) ++ ["test/support"]
  end

  defp elixirc_paths(_), do: ["lib"]

  defp aliases do
    [
      sobelow: "sobelow --skip",
      credo: "credo --strict",
      docs: [
        "spark.cheat_sheets",
        "docs",
        "spark.replace_doc_links"
      ],
      "spark.formatter": "spark.formatter --extensions Reactor.Dsl",
      "spark.cheat_sheets": "spark.cheat_sheets --extensions Reactor.Dsl"
    ]
  end

  defp extra_documentation_groups do
    [
      Tutorials: [
        "documentation/tutorials/01-getting-started.md",
        "documentation/tutorials/02-error-handling.md",
        "documentation/tutorials/03-async-workflows.md",
        "documentation/tutorials/04-composition.md",
        "documentation/tutorials/05-recursive-execution.md",
        "documentation/tutorials/reactor-cheatsheet.cheatmd"
      ],
      "How-to Guides": [
        "documentation/how-to/payment-processing.md",
        "documentation/how-to/data-pipelines.md",
        "documentation/how-to/api-orchestration.md",
        "documentation/how-to/testing-strategies.md",
        "documentation/how-to/debugging-workflows.md",
        "documentation/how-to/performance-optimization.md"
      ],
      Reference: [
        "documentation/dsls/DSL-Reactor.md",
        "documentation/reference/glossary.md"
      ],
      Explanation: [
        "documentation/explanation/concepts.md",
        "documentation/explanation/architecture.md",
        "documentation/explanation/design-decisions.md",
        "documentation/explanation/ecosystem.md"
      ]
    ]
  end
end
