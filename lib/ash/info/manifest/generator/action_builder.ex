# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.ActionBuilder do
  @moduledoc """
  Converts Ash action structs into `%Ash.Info.Manifest.Action{}` structs.
  """

  alias Ash.Info.Manifest.{Action, Argument, Metadata, Pagination}
  alias Ash.Info.Manifest.Generator.TypeResolver

  @doc """
  Build an `%Ash.Info.Manifest.Action{}` from an Ash action struct.

  ## Options

    * `:include_private_arguments?` - Include private arguments (default: `false`)
  """
  @spec build(atom(), struct(), keyword()) :: Action.t()
  def build(resource, action, opts \\ []) do
    %Action{
      name: action.name,
      type: action.type,
      description: Map.get(action, :description),
      primary?: Map.get(action, :primary?, false),
      get?: Map.get(action, :get?, false),
      arguments: build_arguments(action, opts),
      accept: Map.get(action, :accept),
      require_attributes: Map.get(action, :require_attributes),
      allow_nil_input: Map.get(action, :allow_nil_input),
      metadata: build_metadata(resource, action),
      returns: build_returns(action),
      pagination: build_pagination(action)
    }
  end

  defp build_arguments(action, opts) do
    args =
      if Keyword.get(opts, :include_private_arguments?, false) do
        action.arguments
      else
        Enum.filter(action.arguments, & &1.public?)
      end

    Enum.map(args, &build_argument/1)
  end

  defp build_argument(arg) do
    %Argument{
      name: arg.name,
      type: TypeResolver.resolve(arg.type, arg.constraints || []),
      allow_nil?: arg.allow_nil?,
      has_default?: not is_nil(arg.default),
      description: Map.get(arg, :description),
      sensitive?: Map.get(arg, :sensitive?, false)
    }
  end

  defp build_metadata(_resource, action) do
    metadata_list = Map.get(action, :metadata, [])

    Enum.map(metadata_list, fn meta ->
      %Metadata{
        name: meta.name,
        type: TypeResolver.resolve(meta.type, meta.constraints || []),
        allow_nil?: Map.get(meta, :allow_nil?, true),
        description: Map.get(meta, :description)
      }
    end)
  end

  defp build_returns(%{type: :action} = action) do
    case Map.get(action, :returns) do
      nil ->
        nil

      return_type ->
        TypeResolver.resolve(return_type, action.constraints || [])
    end
  end

  defp build_returns(_action), do: nil

  defp build_pagination(%{type: :read, get?: false} = action) do
    case Map.get(action, :pagination) do
      %{} = pagination ->
        %Pagination{
          offset?: Map.get(pagination, :offset?, false),
          keyset?: Map.get(pagination, :keyset?, false),
          required?: Map.get(pagination, :required?, false),
          countable?: countable_to_boolean(Map.get(pagination, :countable, false)),
          default_limit: Map.get(pagination, :default_limit),
          max_page_size: Map.get(pagination, :max_page_size)
        }

      _ ->
        nil
    end
  end

  defp build_pagination(_action), do: nil

  defp countable_to_boolean(true), do: true
  defp countable_to_boolean(:by_default), do: true
  defp countable_to_boolean(_), do: false
end
