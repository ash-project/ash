defmodule Ash.Test.Resource.Validation.NegateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.Negate

  @moduletag :wip

  defmodule Post do
    use Ash.Resource

    attributes do
      uuid_primary_key :id
      attribute :status, :atom
    end
  end

  describe "Negate validation" do
    test "passes when inner validation fails" do
      {:ok, opts} =
        Negate.init(validation: Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]))

      changeset = Post |> Ash.Changeset.new(%{status: :valid})

      assert :ok = Negate.validate(changeset, opts)
    end

    test "fails when inner validation passes" do
      {:ok, opts} =
        Negate.init(validation: Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]))

      changeset = Post |> Ash.Changeset.new(%{status: :canceled})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} = Negate.validate(changeset, opts)
    end
  end
end
