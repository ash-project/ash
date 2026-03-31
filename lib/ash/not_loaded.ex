# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.NotLoaded do
  @moduledoc "Used when a field hasn't been loaded or selected."
  defstruct [:field, :type, :resource]

  @type t :: %__MODULE__{
          field: atom,
          type: :relationship | :calculation | :aggregate | :attribute,
          resource: module | nil
        }

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(not_loaded, opts) do
      custom_options = Map.get(opts, :custom_options, [])
      in_resource? = Keyword.get(custom_options, :in_resource?, false)
      show_resource? = not is_nil(not_loaded.resource) and !in_resource?

      container_doc(
        "#Ash.NotLoaded<",
        [
          to_doc(not_loaded.type, opts),
          concat("field: ", to_doc(not_loaded.field, opts)),
          or_empty(concat("resource: ", to_doc(not_loaded.resource, opts)), show_resource?)
        ],
        ">",
        opts,
        fn doc, _opts -> doc end
      )
    end

    defp or_empty(value, true), do: value
    defp or_empty(_, false), do: empty()
  end
end
