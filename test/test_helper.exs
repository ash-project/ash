# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

# Manifest tests use the dedicated otp_app `:ash_manifest_test` so the
# generator only walks the manifest test domain. Set via `put_env/3`
# (rather than `config :ash_manifest_test, ...` in config/config.exs) to
# avoid Elixir's "application not available" warning for a fake otp_app.
Application.put_env(:ash_manifest_test, :ash_domains, [Ash.Test.Manifest.Domain])

# We compile modules with the same name often while testing the DSL
Mimic.copy(Ash.Reactor.Notifications)
Mimic.copy(Ash.DataLayer)
Mimic.copy(Ash.ProcessHelpers)

Code.compiler_options(ignore_module_conflict: true)

ExUnit.start(stacktrace_depth: 100)
