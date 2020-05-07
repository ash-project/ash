defmodule Ash.DslBuilder do
  defmacro build_dsl(keys) do
    quote bind_quoted: [keys: keys] do
      for key <- keys do
        defmacro unquote(key)(value) do
          key = unquote(key)

          quote do
            @dsl_opts {unquote(key), unquote(value)}
          end
        end
      end
    end
  end
end
