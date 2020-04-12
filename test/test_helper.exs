ExUnit.start()
Logger.configure(level: :error)

# We compile modules with the same name often while testing the DSL
Code.compiler_options(ignore_module_conflict: true)
