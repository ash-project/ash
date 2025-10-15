# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyPrimaryReadActionHasNoArguments do
  @moduledoc "Verifies that primary read actions do not have any arguments"
  use Spark.Dsl.Verifier

  def verify(dsl) do
    resource = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    if Spark.Dsl.Verifier.get_persisted(dsl, :primary_read_warning?) do
      dsl
      |> Ash.Resource.Info.actions()
      |> Enum.find(&(&1.type == :read && &1.primary?))
      |> case do
        nil ->
          :ok

        action ->
          if action.arguments != [] do
            IO.warn(warning(resource, action, "arguments"))
          end

          if action.filter not in [nil, []] do
            IO.warn(warning(resource, action, "filters"))
          end

          if action.preparations != [] &&
               Ash.Resource.Info.data_layer(resource) != Ash.DataLayer.Simple do
            IO.warn(warning(resource, action, "preparations"))
          end

          :ok
      end
    else
      :ok
    end
  end

  defp warning(resource, action, things) do
    """
    Primary read action `#{inspect(resource)}.#{action.name}` has #{things}.

    This is often done by mistake, but can also be done intentionally.

    Primary read actions are used when loading relationships, checking policies and more.
    #{if things == "arguments", do: "It is okay to have optional arguments, but required arguments are almost never desired.\n", else: ""}
    If you intended to have #{things} on your primary read action, add `primary_read_warning?: false`
    to `use Ash.Resource`. For example:

        use Ash.Resource,
          primary_read_warning?: false
    """
  end
end
