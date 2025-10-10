
# Manual Installation
This guide will walk you through the process of manually installing Ash into your project.
If you are starting from scratch, you can use `mix new` or `mix igniter.new` and follow these instructions.
These installation instructions apply both to new projects and existing ones.

## Install & Setup Dependencies
See the readmes for `spark` and `reactor` for more information on their installation.
We've included their changes here for your convenience.

Create `.formatter.exs`:
```
# Used by "mix format"
[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  plugins: [Spark.Formatter]
]

```

Create `config/config.exs`:
```
import Config
config :spark, formatter: [remove_parens?: true]

```

Update `mix.exs`:
```diff
...
    defp deps do
      [
+       {:sourceror, "~> 1.8", only: [:dev, :test]}
        # {:dep_from_hexpm, "~> 0.3.0"},
        # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
...
```

Update `.formatter.exs`:
```diff
...
  [
    inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
-   plugins: [Spark.Formatter]
+   plugins: [Spark.Formatter],
+   import_deps: [:reactor]
  ]
```

## Skip protocol consolidation
To avoid warnings about protocol consolidation when recompiling in dev, we
set protocolc onsolidation to happen only in non-dev environments.

## Setup The Formatter
Configure the DSL auto-formatter. This tells the formatter to remove excess parentheses
and how to sort sections in your Ash.Resource & Ash.Domain modules for consistency.

Update `.formatter.exs`:
```diff
...
    inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
    plugins: [Spark.Formatter],
-   import_deps: [:reactor]
+   import_deps: [:ash, :reactor]
  ]
```

Update `config/config.exs`:
```diff
  import Config
- config :spark, formatter: [remove_parens?: true]
  
+ config :spark,
+   formatter: [
+     remove_parens?: true,
+     "Ash.Resource": [
+       section_order: [
+         :resource,
+         :code_interface,
+         :actions,
+         :policies,
+         :pub_sub,
+         :preparations,
+         :changes,
+         :validations,
+         :multitenancy,
+         :attributes,
+         :relationships,
+         :calculations,
+         :aggregates,
+         :identities
+       ]
+     ],
+     "Ash.Domain": [section_order: [:resources, :policies, :authorization, :domain, :execution]]
+   ]
+
```

## Configure Dev/Test environments
Configure backwards compatibility settings. See the [backwards compatibility guide](https://hexdocs.pm/ash/backwards-compatibility-config.html)
for an explanation of each of the configurations.

Update `config/config.exs`:
```diff
...
    ]
  
+ import_config "#{config_env()}.exs"
+
```

Create `config/dev.exs`:
```
import Config
config :ash, policies: [show_policy_breakdowns?: true]

```

Create `config/prod.exs`:
```
import Config

```

Create `config/test.exs`:
```
import Config
config :ash, policies: [show_policy_breakdowns?: true]

```

## Setup Backwards Compatibility Configurations
Configure backwards compatibility settings. See the [backwards compatibility guide](https://hexdocs.pm/ash/backwards-compatibility-config.html)
for an explanation of each of the configurations.

Update `config/config.exs`:
```diff
  import Config
  
+ config :ash,
+   allow_forbidden_field_for_relationships_by_default?: true,
+   include_embedded_source_by_default?: false,
+   show_keysets_for_all_actions?: false,
+   default_page_type: :keyset,
+   policies: [no_filter_static_forbidden_reads?: false],
+   keep_read_action_loads_when_loading?: false,
+   default_actions_require_atomic?: true,
+   read_action_after_action_hooks_in_order?: true,
+   bulk_actions_default_to_errors?: true,
+   transaction_rollback_on_error?: true
+ 
  config :spark,
    formatter: [
...
```
