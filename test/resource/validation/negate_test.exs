defmodule Ash.Test.Resource.Validation.NegateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Validation.Negate

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain

    actions do
      defaults [:create, :update, :destroy, :read]
    end

    attributes do
      uuid_primary_key :id
      attribute :status, :atom
    end
  end

  defmodule CustomValidation do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: {:error, :some_error}

    @impl true
    def describe(_opts), do: [message: "Custom validation error message", vars: []]
  end

  defmodule CustomValidationNoDescribe do
    use Ash.Resource.Validation

    @impl true
    def validate(_, _, _), do: {:error, :some_error}
  end

  describe "Negate validation" do
    test "passes when inner validation fails" do
      {:ok, opts} =
        Negate.init(validation: Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]))

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = Negate.validate(changeset, opts, %{})
    end

    test "fails when inner validation passes" do
      {:ok, opts} =
        Negate.init(validation: Ash.Resource.Validation.Builtins.one_of(:status, [:canceled]))

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :canceled})

      assert {:error, %Ash.Error.Changes.InvalidAttribute{}} =
               Negate.validate(changeset, opts, %{})
    end

    test "support custom validations" do
      {:ok, opts} = Negate.init(validation: CustomValidation)

      changeset = Post |> Ash.Changeset.for_create(:create, %{status: :valid})

      assert :ok = Negate.validate(changeset, opts, %{})
    end

    test "returns error on init if validation do not export `describe/1`" do
      assert_raise ArgumentError, ~r/must implement `describe\/1`/, fn ->
        Negate.init(validation: CustomValidationNoDescribe)
      end
    end
  end
end
