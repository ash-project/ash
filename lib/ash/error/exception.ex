defmodule Ash.Error.Exception do
  @moduledoc "Tooling for creating an Ash exception"

  defmacro __using__(_) do
    quote do
      import Ash.Error.Exception, only: [def_ash_error: 1, def_ash_error: 2]
    end
  end

  defmacro def_ash_error(fields, opts \\ []) do
    quote location: :keep, generated: true do
      defexception unquote(fields) ++
                     [
                       :changeset,
                       :query,
                       error_context: [],
                       vars: [],
                       path: [],
                       stacktrace: nil,
                       class: unquote(opts)[:class]
                     ]

      def new(opts), do: exception(opts)

      @impl Exception
      def message(%{vars: vars} = exception) do
        string = Ash.ErrorKind.message(exception)

        string =
          case Ash.Error.breadcrumb(Map.get(exception, :error_context)) do
            "" ->
              string

            context ->
              context <> "\n" <> string
          end

        Enum.reduce(List.wrap(vars), string, fn {key, value}, acc ->
          if String.contains?(acc, "%{#{key}}") do
            String.replace(acc, "%{#{key}}", to_string(value))
          else
            acc
          end
        end)
      end

      def exception(opts) do
        opts =
          Keyword.put_new_lazy(opts, :stacktrace, fn ->
            {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

            %{
              __struct__: Ash.Error.Stacktrace,
              stacktrace: Enum.drop(stacktrace, 4)
            }
          end)

        super(opts)
      end

      defoverridable exception: 1, message: 1
    end
  end
end
