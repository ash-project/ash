# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

# We compile modules with the same name often while testing the DSL
Mimic.copy(Ash.Reactor.Notifications)
Mimic.copy(Ash.DataLayer)
Mimic.copy(Ash.ProcessHelpers)

Code.compiler_options(ignore_module_conflict: true)

ExUnit.start(stacktrace_depth: 100)
