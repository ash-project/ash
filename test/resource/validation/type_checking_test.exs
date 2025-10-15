# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.TypeCheckingTest do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ValidValidation do
    use Ash.Resource.Validation

    def validate(_changeset, _opts, _context) do
      :ok
    end
  end

  defmodule ValidErrorValidation do
    use Ash.Resource.Validation

    def validate(_changeset, _opts, _context) do
      {:error, "validation failed"}
    end
  end

  defmodule InvalidReturnValidation do
    use Ash.Resource.Validation

    def validate(_changeset, _opts, _context) do
      "this should be :ok or {:error, _}"
    end
  end

  defmodule Resource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create_with_valid_validation do
        validate ValidValidation
      end

      create :create_with_valid_error_validation do
        validate ValidErrorValidation
      end

      create :create_with_invalid_validation do
        validate InvalidReturnValidation
      end
    end
  end

  test "valid validation returning :ok works" do
    changeset = Ash.Changeset.for_create(Resource, :create_with_valid_validation)
    context = struct(Ash.Resource.Validation.Context, %{})

    result = Ash.Resource.Validation.validate(ValidValidation, changeset, [], context)
    assert result == :ok
  end

  test "valid validation returning {:error, _} works" do
    changeset = Ash.Changeset.for_create(Resource, :create_with_valid_error_validation)
    context = struct(Ash.Resource.Validation.Context, %{})

    result = Ash.Resource.Validation.validate(ValidErrorValidation, changeset, [], context)
    assert {:error, "validation failed"} = result
  end

  test "invalid validation returning wrong type raises helpful error" do
    changeset = Ash.Changeset.for_create(Resource, :create_with_valid_validation)
    context = struct(Ash.Resource.Validation.Context, %{})

    assert_raise Ash.Error.Framework.InvalidReturnType,
                 ~r/Invalid value returned from.*validate\/3/,
                 fn ->
                   Ash.Resource.Validation.validate(
                     InvalidReturnValidation,
                     changeset,
                     [],
                     context
                   )
                 end
  end

  test "type checking works through normal action flow" do
    # Valid validation returning :ok should work
    assert %Resource{} = Ash.create!(Resource, %{}, action: :create_with_valid_validation)

    # Valid validation returning {:error, _} should cause action to fail
    assert_raise Ash.Error.Invalid, fn ->
      Ash.create!(Resource, %{}, action: :create_with_valid_error_validation)
    end

    # Invalid validation should fail during action execution with type error
    assert_raise Ash.Error.Framework, ~r/Invalid value returned from.*validate\/3/, fn ->
      Ash.create!(Resource, %{}, action: :create_with_invalid_validation)
    end
  end
end
