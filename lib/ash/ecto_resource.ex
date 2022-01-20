defmodule Ash.EctoResource do
  defmacro __using__(opts) do
    quote do
      use Ash.Resource
      @source_schema unquote(opts[:schema])

      attributes do
        for field <- @source_schema.__schema__(:fields) do
          attribute field, @source_schema.__schema__(:type, field)
        end
      end
    end
  end
end
