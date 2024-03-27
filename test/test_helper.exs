# We compile modules with the same name often while testing the DSL
Mimic.copy(Ash.Reactor.Notifications)
Mimic.copy(Ash.DataLayer)

Code.compiler_options(ignore_module_conflict: true)

ExUnit.start(stacktrace_depth: 100)
