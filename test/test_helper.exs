:erlang.system_flag(:backtrace_depth, 100)
ExUnit.start()
Logger.configure(level: :debug)

# We compile modules with the same name often while testing the DSL
Code.compiler_options(ignore_module_conflict: true)
