defmodule Ash.Error.Exception do
  @moduledoc "Tooling for creating an Ash exception"

  defmacro __using__(_) do
    quote do
      import Ash.Error.Exception, only: [def_ash_error: 1, def_ash_error: 2]
    end
  end

  defmacro def_ash_error(fields, opts \\ []) do
    quote do
      defexception unquote(fields) ++
                     [
                       :changeset,
                       :query,
                       vars: [],
                       private_vars: [],
                       path: [],
                       stacktrace: [],
                       class: unquote(opts)[:class]
                     ]

      @impl Exception

      def message(%{vars: vars, private_vars: private_vars} = exception) do
        string = Ash.ErrorKind.message(exception)

        Enum.reduce(List.wrap(vars) ++ List.wrap(private_vars), string, fn {key, value}, acc ->
          if String.contains?(acc, "%{#{key}}") do
            String.replace(acc, "%{#{key}}", to_string(value))
          else
            acc
          end
        end)
      end

      def exception(opts) do
        case Process.info(self(), :current_stacktrace) do
          {:current_stacktrace, [_, _ | stacktrace]} ->
            super(
              Keyword.put_new(opts, :stacktrace, %Ash.Error.Stacktrace{stacktrace: stacktrace})
            )

          _ ->
            super(opts)
        end
      end

      defoverridable exception: 1, message: 1
    end
  end
end
