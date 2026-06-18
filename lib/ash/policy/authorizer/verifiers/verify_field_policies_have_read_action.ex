# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Authorizer.Verifiers.VerifyFieldPoliciesHaveReadAction do
  @moduledoc false
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  def verify(dsl) do
    module = Verifier.get_persisted(dsl, :module)

    if Ash.Resource.Info.resource?(module) and has_field_policies?(dsl) and
         not has_primary_read_action?(dsl) do
      location =
        try do
          Spark.Dsl.Extension.get_section_anno(dsl, [:field_policies])
        rescue
          _ -> nil
        end

      {:error,
       Spark.Error.DslError.exception(
         module: module,
         location: location,
         path: [:field_policies],
         message: """
         `field_policies` were defined on `#{inspect(module)}`, but the
         resource has no primary read action.

         Field policies are applied through the read pipeline — they get
         injected as calculations on the underlying read query whenever a
         record is loaded. Without a primary read action there is no
         pipeline for that injection to run through, which means the
         policies you defined would silently fail to scrub forbidden fields.

         Add a primary read action, for example:

             actions do
               defaults [:read]
             end

         or mark one of your existing read actions as `primary?: true`.
         """
       )}
    else
      :ok
    end
  end

  defp has_field_policies?(dsl) do
    case Verifier.get_entities(dsl, [:field_policies]) do
      [] -> false
      [_ | _] -> true
    end
  end

  defp has_primary_read_action?(dsl) do
    dsl
    |> Verifier.get_entities([:actions])
    |> Enum.any?(fn action ->
      Map.get(action, :type) == :read and Map.get(action, :primary?, false)
    end)
  end
end
