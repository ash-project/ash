# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TypedStruct.Info do
  @moduledoc "Introspection for typed structs"

  @doc "Returns all fields of a typed struct"
  @spec fields(Spark.Dsl.t() | Ash.Type.NewType.t()) :: [Ash.TypedStruct.Field.t()]
  def fields(typed_struct) do
    Spark.Dsl.Extension.get_entities(typed_struct, [:typed_struct])
  end

  @spec field_names(Spark.Dsl.t() | Ash.Type.NewType.t()) :: list(atom)
  def field_names(typed_struct) do
    typed_struct
    |> fields()
    |> Enum.into([], &Map.get(&1, :name))
  end

  @doc "Get a field by name from the typed struct"
  @spec field(Spark.Dsl.t() | Ash.Type.NewType.t(), String.t() | atom) ::
          Ash.TypedStruct.Field.t() | nil
  def field(typed_struct, name) when is_binary(name) or is_atom(name) do
    typed_struct
    |> fields()
    |> Enum.find(&(to_string(&1.name) == to_string(name)))
  end

  @doc """
  Returns a list of extensions in use by the typed struct
  """
  @spec extensions(typed_struct :: Ash.Type.NewType.t()) :: list(module())
  defdelegate extensions(typed_struct), to: Spark
end
