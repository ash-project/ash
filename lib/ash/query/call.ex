defmodule Ash.Query.Call do
  @moduledoc "Represents a function call/AST node in an Ash query expression"

  defstruct [:name, :args, operator?: false]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(call, inspect_opts) do
      if call.operator? do
        concat([
          to_doc(Enum.at(call.args, 0), inspect_opts),
          " ",
          to_string(call.name),
          " ",
          to_doc(Enum.at(call.args, 1), inspect_opts)
        ])
      else
        concat([
          to_string(call.name),
          container_doc("(", call.args, ")", inspect_opts, &to_doc/2, separator: ", ")
        ])
      end
    end
  end
end
