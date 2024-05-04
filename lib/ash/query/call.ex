defmodule Ash.Query.Call do
  @moduledoc "Represents a function call/AST node in an Ash query expression"

  defstruct [:name, :args, relationship_path: [], operator?: false]

  defimpl Inspect do
    import Inspect.Algebra
    import Ash.Query.InspectHelpers

    def inspect(%{name: :if, operator?: false, args: args}, opts) do
      {:ok, if_expr} = Ash.Query.Function.If.new(args)
      to_doc(if_expr, opts)
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
            Enum.map_join(call.relationship_path, ".", fn item ->
              case item do
                %struct{} when struct in [Ash.Query.Aggregate, Ash.Query.Calculation] ->
                  inspect(item)

                %{name: name} ->
                  name

                name ->
                  name
              end
            end) <> "."
          end

        args =
          case call.args do
            %{ash: _} ->
              Map.delete(call.args, :ash)

            _ ->
              call.args
          end

        case args do
          [arg] ->
            if Keyword.keyword?(arg) do
              concat([
                prefix,
                to_string(call.name),
                container_doc("(", args, ")", inspect_opts, &to_doc/2, separator: ", ")
              ])
            else
              concat([
                prefix,
                to_string(call.name),
                container_doc("(", args, ")", inspect_opts, &to_doc/2, separator: ", ")
              ])
            end

          args ->
            concat([
              prefix,
              to_string(call.name),
              container_doc("(", args, ")", inspect_opts, &to_doc/2, separator: ", ")
            ])
        end
      end
    end
  end
end
