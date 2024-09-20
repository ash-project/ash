defmodule Ash.Policy.PolicyGroup do
  @moduledoc "Represents a policy group on an Ash.Resource"

  # For now we just write to `checks` and move them to `policies`
  # on build, when we support nested policies we can change that.
  defstruct [
    :condition,
    :policies
  ]

  @doc false
  def transform(group) do
    if Enum.empty?(group.policies) do
      {:error, "Policy groups must contain at least one policy."}
    else
      if Enum.any?(group.policies, fn
           %{bypass?: bypass?} ->
             bypass?

           _ ->
             false
         end) do
        {:error, "Policy groups cannot contain bypass policies."}
      else
        if group.condition in [nil, []] do
          {:ok, %{group | condition: [{Ash.Policy.Check.Static, result: true}]}}
        else
          {:ok, group}
        end
      end
    end
  end
end
