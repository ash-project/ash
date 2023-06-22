defmodule Ash.Policy.FieldPolicy do
  defstruct [
    :fields,
    :condition,
    :policies,
    :description,
    :__identifier__
  ]

  @type t :: %__MODULE__{}

  @doc false
  def transform(field_policy) do
    if Enum.empty?(field_policy.policies) do
      {:error, "Field policies must have at least one check."}
    else
      {:ok, %{field_policy | policies: Enum.map(field_policy.policies, &set_field_policy_opt/1)}}
    end
  end

  defp set_field_policy_opt(%{check_opts: opts} = policy) do
    %{policy | check_opts: Keyword.put(opts, :ash_field_policy?, true)}
  end
end
