defmodule Ash.Flow.Step.BuiltinStep do
  @moduledoc false
  defmacro __using__(fields) do
    quote do
      defstruct [
                  :name,
                  :wait_for,
                  :halt_if,
                  :halt_reason,
                  :description,
                  :short_name,
                  touches_resources: []
                ] ++
                  unquote(fields)
    end
  end
end
