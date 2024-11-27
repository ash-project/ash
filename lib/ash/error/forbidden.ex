defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :forbidden

  @type t :: %__MODULE__{
          changeset: Ash.Changeset.t() | nil,
          query: Ash.Query.t() | nil,
          action_input: Ash.ActionInput.t() | nil
        }
  defimpl Inspect do
    import Inspect.Algebra

    def inspect(
          %{
            changeset: changeset,
            query: query,
            action_input: action_input,
            errors: errors,
            bread_crumbs: bread_crumbs,
            vars: vars,
            path: path
          },
          opts
        ) do
      changeset =
        if changeset do
          "#Changeset<>"
        else
          nil
        end

      query =
        if query do
          "#Query<>"
        else
          nil
        end

      action_input =
        if action_input do
          "#ActionInput<>"
        else
          nil
        end

      items =
        [
          bread_crumbs: bread_crumbs,
          path: path,
          vars: vars,
          changeset: changeset,
          query: query,
          action_input: action_input,
          errors: errors
        ]
        |> Enum.reject(fn {_key, item} ->
          item in [nil, []]
        end)
        |> Enum.map(fn {key, value} ->
          {key, to_doc(value, opts)}
        end)

      container_doc(
        "%Ash.Error.Forbidden{",
        items,
        "}",
        opts,
        fn {key, val}, _ -> concat(["#{key}: ", val]) end,
        separator: ", "
      )
    end
  end
end
