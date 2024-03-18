defmodule Ash.Error.Exception do
  @moduledoc "Tooling for creating an Ash exception"

  defmacro __using__(_) do
    quote do
      import Ash.Error.Exception, only: [def_ash_error: 1, def_ash_error: 2]
    end
  end

  defmacro def_ash_error(fields, opts \\ []) do
    quote location: :keep, generated: true, bind_quoted: [fields: fields, opts: opts] do
      IO.warn("""
      def_ash_error is deprecated. Instead, use `Splode.Error`, and
      remove any usage of `Ash.ErrorKind`. Place your `message/1` function
      into the module body as `def message/1`. For example:

      ```elixir
      use Splode.Error,
        fields: #{inspect(fields)},
        class: #{inspect(opts[:class])}

      def message(error) do
        ...your_message
      end
      ```
      """)

      use Splode.Error,
        fields: fields,
        class: opts[:class]

      def message(exception) do
        Ash.ErrorKind.message(exception)
      end
    end
  end
end
