defmodule Ash.Query.Call do
  @moduledoc "Represents a function call/AST node in an Ash query expression"

  defstruct [:name, :args, relationship_path: [], operator?: false]

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
        prefix =
          if call.relationship_path == [] do
            ""
          else
            Enum.map_join(call.relationship_path, ".", &to_string/1) <> "."
          end

        concat([
          prefix,
          to_string(call.name),
          container_doc("(", call.args, ")", inspect_opts, &to_doc/2, separator: ", ")
        ])
      end
    end
  end
end
