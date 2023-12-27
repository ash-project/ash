defmodule Ash.Query.Call do
  @moduledoc "Represents a function call/AST node in an Ash query expression"

  defstruct [:name, :args, relationship_path: [], operator?: false]

  defimpl Inspect do
    import Inspect.Algebra
    import Ash.Query.InspectHelpers

    def inspect(%{name: :if, operator?: false, args: [condition, blocks]} = call, opts) do
      if Keyword.keyword?(blocks) do
        if Keyword.has_key?(blocks, :else) do
          concat([
            nest(
              concat([
                group(concat(["if ", to_doc(condition, opts), " do"])),
                line(),
                to_doc(blocks[:do], opts)
              ]),
              2
            ),
            line(),
            "else",
            nest(
              concat([
                line(),
                to_doc(blocks[:else], opts)
              ]),
              2
            ),
            line(),
            "end"
          ])
        else
          concat([
            nest(
              concat([
                group(concat(["if ", to_doc(condition, opts), " do"])),
                line(),
                to_doc(blocks[:do], opts)
              ]),
              2
            ),
            line(),
            "end"
          ])
        end
      else
        do_inspect(call, opts)
      end
    end

    def inspect(call, inspect_opts) do
      do_inspect(call, inspect_opts)
    end

    defp do_inspect(call, inspect_opts) do
      container_type = container_type(inspect_opts)

      if call.operator? do
        if container_type do
          concat([
            "(",
            to_doc(
              Enum.at(call.args, 0),
              inspect_opts
            ),
            " ",
            to_string(call.name),
            " ",
            to_doc(Enum.at(call.args, 1), inspect_opts),
            ")"
          ])
        else
          concat([
            to_doc(
              Enum.at(call.args, 0),
              inspect_opts
            ),
            " ",
            to_string(call.name),
            " ",
            to_doc(Enum.at(call.args, 1), inspect_opts)
          ])
        end
      else
        prefix =
          if call.relationship_path == [] do
            ""
          else
            Enum.map_join(call.relationship_path, ".", &to_string/1) <> "."
          end

        args =
          case call.args do
            %{ash: _} ->
              Map.delete(call.args, :ash)

            _ ->
              call.args
          end

        concat([
          prefix,
          to_string(call.name),
          container_doc("(", args, ")", inspect_opts, &to_doc/2, separator: ", ")
        ])
      end
    end
  end
end
