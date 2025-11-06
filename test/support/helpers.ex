# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Helpers do
  @moduledoc false

  defmacro defposts(do: body) do
    quote do
      {:module, mod, _, _} =
        defmodule Module.concat(["rand#{System.unique_integer([:positive])}", Post]) do
          @moduledoc false
          use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
          end

          unquote(body)
        end

      mod
    end
  end

  defmacro hydrated_expr(resource, expr) do
    quote do
      require Ash.Expr

      Ash.Expr.expr(unquote(expr))
      |> Ash.Filter.hydrate_refs(%{
        resource: unquote(resource),
        aggregates: %{},
        calculations: %{},
        public?: false
      })
    end
  end

  @doc """
  Strips bulk_action_ref metadata from records to simulate legacy data layer behavior.

  Use this in tests that verify backwards compatibility with data layers that don't
  support returning the bulk action ref.
  """
  def strip_bulk_action_refs(%Ash.BulkResult{records: records} = result)
      when is_list(records) do
    %{result | records: Enum.map(records, &strip_bulk_action_ref/1)}
  end

  def strip_bulk_action_refs(result), do: result

  def strip_bulk_action_ref(record) when is_struct(record) do
    Ash.Resource.set_metadata(record, Map.delete(record.__metadata__, :bulk_action_ref))
  end

  def strip_bulk_action_ref(other), do: other
end
