# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# We compile modules with the same name often while testing the DSL
Mimic.copy(Ash.Reactor.Notifications)
Mimic.copy(Ash.DataLayer)
Mimic.copy(Ash.ProcessHelpers)

Code.compiler_options(ignore_module_conflict: true)

ExUnit.start(stacktrace_depth: 100)
