exclude = [
  ash_three: System.get_env("FLAG_ASH_THREE", "false") != "true"
]

ExUnit.start(stacktrace_depth: 100, exclude: exclude)
Logger.configure(level: :debug)

# We compile modules with the same name often while testing the DSL
Code.compiler_options(ignore_module_conflict: true)
