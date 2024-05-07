defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :unknown

  @type t :: %__MODULE__{}

  def exception(opts) do
    if opts[:error] do
      super(Keyword.update(opts, :errors, [opts[:error]], &[opts[:error] | &1]))
    else
      super(opts)
    end
  end

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
        "%Ash.Error.Unknown{",
        items,
        "}",
        opts,
        fn {key, val}, _ -> concat(["#{key}: ", val]) end,
        separator: ", "
      )
    end
  end
end
