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

      def from_json(json) do
        keyword =
          json
          |> Map.to_list()
          |> Enum.map(fn {key, value} -> {Ash.Error.atomize_safely(key), value} end)

        exception(keyword)
      end

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
          if is_nil(opts[:stacktrace]) do
            {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

            stacktrace =
              %{
                __struct__: Ash.Error.Stacktrace,
                stacktrace: stacktrace
              }

            Keyword.put(opts, :stacktrace, stacktrace)
          else
            opts
          end

        super(opts) |> Map.update(:vars, [], &clean_vars/1)
      end

      defp clean_vars(vars) when is_map(vars) do
        clean_vars(Map.to_list(vars))
      end

      defp clean_vars(vars) do
        vars |> Kernel.||([]) |> Keyword.drop([:field, :message, :path])
      end

      defoverridable exception: 1, message: 1, from_json: 1
    end
  end
end
