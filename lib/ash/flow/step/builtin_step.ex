defmodule Ash.Flow.Step.BuiltinStep do
  @moduledoc false
  defmacro __using__(fields) do
    quote do
      defstruct [:name, :wait_for, touches_resources: []] ++ unquote(fields)
    end
  end
end
