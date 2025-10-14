# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Authorizer.Verifiers.VerifyResources do
  @moduledoc false
  use Spark.Dsl.Verifier

  def verify(dsl) do
    dsl
    |> Ash.Domain.Info.resources()
    |> Enum.reject(fn resource ->
      Ash.Policy.Authorizer in Ash.Resource.Info.authorizers(resource)
    end)
    |> case do
      [] ->
        :ok

      resources ->
        domain = Spark.Dsl.Verifier.get_persisted(dsl, :module)

        IO.warn("""
        Policies were defined on the domain `#{inspect(domain)}` but not on all resources.

        Domain policies are not applied to resources with no policies of their own.

        To address this, add the `Ash.Policy.Authorizer` authorizer to the
        following resources.

        #{Enum.map_join(resources, "\n", &"* #{inspect(&1)}")}

        The following can be added to resources that have no policy rules
        of their own like so:

            policies do
              policy always() do
                authorize_if always()
              end
            end

        All policies that apply to a request must pass, so the above policies
        will not prevent the domain's policies from being applied.
        """)

        :ok
    end
  end
end
