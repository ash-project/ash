# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Ref do
  @moduledoc "Represents a relation/attribute reference"
  defstruct [
    :attribute,
    :resource,
    :simple_equality?,
    :bare?,
    :input?,
    combinations?: false,
    relationship_path: []
  ]

  @doc "Returns the referenced field"
  def name(%__MODULE__{attribute: %{name: name}}), do: name
  def name(%__MODULE__{attribute: name}), do: name

  defimpl Inspect do
    def inspect(ref, _opts) do
      name =
        case ref.attribute do
          %mod{} when mod in [Ash.Query.Aggregate, Ash.Query.Calculation] ->
            ref.attribute

          %{name: name} ->
            name

          name ->
            name
        end

      name =
        if is_binary(name) or is_atom(name) do
          to_string(name)
        else
          inspect(name)
        end

      case ref.relationship_path do
        [] ->
          name

        path ->
          Enum.map_join(path, ".", fn item ->
            case item do
              %struct{} when struct in [Ash.Query.Aggregate, Ash.Query.Calculation] ->
                inspect(item)

              %{name: name} ->
                name

              name ->
                name
            end
          end) <> "." <> "#{name}"
      end
      |> then(fn value ->
        if ref.combinations? do
          "combinations(#{value})"
        else
          value
        end
      end)
    end
  end
end
