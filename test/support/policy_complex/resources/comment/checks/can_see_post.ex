defmodule Ash.Test.Support.PolicyComplex.Comment.Checks.ManualCanSeePost do
  @moduledoc false
  use Ash.Policy.Check

  require Ash.Query

  def describe(_), do: "can view the related post"

  def strict_check(actor, %{changeset: %Ash.Changeset{} = changeset}, _opts) do
    post_id =
      changeset
      |> Ash.Changeset.get_argument(:post_id)
      |> Kernel.||(raise "Must have post_id argument set to use `strict_check/3`")

    Ash.Test.Support.PolicyComplex.Post
    |> Ash.Query.filter(id == ^post_id)
    |> Ash.Test.Support.PolicyComplex.Api.read_one!(
      actor: actor,
      authorize?: true
    )
    |> case do
      nil ->
        {:ok, false}

      _ ->
        {:ok, true}
    end
  end

  def strict_check(_, _, _), do: {:ok, :unknown}
end
