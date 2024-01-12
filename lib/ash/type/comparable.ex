defmodule Ash.Type.Comparable do
  @moduledoc "Helpers for working with `Comparable`"
  defmacro defcomparable(
             {:"::", _, [left_expression, quoted_left_type]},
             {:"::", _, [right_expression, quoted_right_type]},
             do: code
           ) do
    {left_type, []} = Code.eval_quoted(quoted_left_type, [], __CALLER__)

    {right_type, []} = Code.eval_quoted(quoted_right_type, [], __CALLER__)

    lr_type =
      [Comparable, Type, left_type, To, right_type]
      |> Module.concat()

    rl_type =
      [Comparable, Type, right_type, To, left_type]
      |> Module.concat()

    lr_impl =
      quote do
        defmodule unquote(lr_type) do
          @fields [:left, :right]
          @enforce_keys @fields
          defstruct @fields
        end

        defimpl Comparable, for: unquote(lr_type) do
          @moduledoc false
          def compare(%unquote(lr_type){
                left: unquote(left_expression),
                right: unquote(right_expression)
              }) do
            unquote(code)
          end
        end
      end

    if lr_type == rl_type do
      lr_impl
    else
      quote do
        unquote(lr_impl)

        defmodule unquote(rl_type) do
          @fields [:left, :right]
          @enforce_keys @fields
          defstruct @fields
        end

        defimpl Comparable, for: unquote(rl_type) do
          @moduledoc false
          def compare(%unquote(rl_type){
                left: unquote(right_expression),
                right: unquote(left_expression)
              }) do
            case unquote(code) do
              :gt -> :lt
              :lt -> :gt
              :eq -> :eq
            end
          end
        end
      end
    end
  end
end
