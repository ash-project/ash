# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.NegateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Resource.Validation.Negate

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :status, :atom do
        public?(true)
      end
    end

    validations do
      validate negate(changing(:status)),
        on: :update,
        where: [data_one_of(:status, [:published])],
        message: "The status of a published post cannot be changed."
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

    test "conditionally rejects changing an attribute in an atomic update" do
      draft = Ash.create!(Post, %{status: :draft})
      published = Ash.create!(Post, %{status: :published})

      assert %Ash.BulkResult{
               status: :success,
               records: [%Post{status: :archived}]
             } =
               Post
               |> Ash.Query.filter(id == ^draft.id)
               |> Ash.bulk_update(:update, %{status: :archived},
                 strategy: :atomic,
                 return_records?: true,
                 return_errors?: true
               )

      assert %Ash.BulkResult{
               status: :error,
               errors: [
                 %Ash.Error.Invalid{
                   errors: [
                     %Ash.Error.Changes.InvalidChanges{
                       fields: [:status],
                       message: "The status of a published post cannot be changed."
                     }
                   ]
                 }
               ]
             } =
               Post
               |> Ash.Query.filter(id == ^published.id)
               |> Ash.bulk_update(:update, %{status: :archived},
                 strategy: :atomic,
                 return_errors?: true
               )
    end
  end
end
