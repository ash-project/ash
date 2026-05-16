# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.ValidatorsTest.MixedAcceptResource do
  @moduledoc false
  use Ash.Resource, domain: nil

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true
    attribute :internal_token, :string, public?: false
    attribute :other_private, :string, public?: false
  end

  actions do
    create :create_all do
      accept [:name, :internal_token, :other_private]
    end

    create :create_public_only do
      accept [:name]
    end

    create :create_no_accept

    update :update_internal do
      accept [:internal_token]
    end
  end
end

defmodule Ash.Test.Info.Manifest.ValidatorsTest do
  use ExUnit.Case, async: true

  alias Ash.Info.Manifest.Validators
  alias Ash.Test.Info.Manifest.ValidatorsTest.MixedAcceptResource

  defp action(name), do: Ash.Resource.Info.action(MixedAcceptResource, name)

  describe "validate_accept_public/2" do
    test "returns :ok when accept contains only public attributes" do
      assert :ok =
               Validators.validate_accept_public(MixedAcceptResource, action(:create_public_only))
    end

    test "returns :ok when accept is nil" do
      assert :ok =
               Validators.validate_accept_public(MixedAcceptResource, action(:create_no_accept))
    end

    test "returns :ok for a plain map with no :accept key" do
      assert :ok = Validators.validate_accept_public(MixedAcceptResource, %{})
    end

    test "returns {:error, attrs} listing every non-public attribute in accept" do
      result = Validators.validate_accept_public(MixedAcceptResource, action(:create_all))

      assert {:error, attrs} = result
      assert Enum.sort(attrs) == [:internal_token, :other_private]
    end

    test "works with %Ash.Info.Manifest.Action{}-shaped maps" do
      spec_action = %{accept: [:name, :internal_token]}

      assert {:error, [:internal_token]} =
               Validators.validate_accept_public(MixedAcceptResource, spec_action)
    end

    test "flags non-public attributes on update actions too" do
      assert {:error, [:internal_token]} =
               Validators.validate_accept_public(MixedAcceptResource, action(:update_internal))
    end
  end

  describe "validate_entrypoint!/2" do
    test "returns :ok when all accepted attributes are public" do
      assert :ok =
               Validators.validate_entrypoint!(MixedAcceptResource, action(:create_public_only))
    end

    test "returns :ok when accept is nil" do
      assert :ok = Validators.validate_entrypoint!(MixedAcceptResource, action(:create_no_accept))
    end

    test "raises NonPublicAccept with resource, action, and offending attrs" do
      err =
        assert_raise Ash.Info.Manifest.Error.NonPublicAccept, fn ->
          Validators.validate_entrypoint!(MixedAcceptResource, action(:create_all))
        end

      assert err.resource == MixedAcceptResource
      assert err.action == :create_all
      assert Enum.sort(err.attributes) == [:internal_token, :other_private]
      assert err.message =~ "create_all"
      assert err.message =~ "internal_token"
      assert err.message =~ "public?: true"
    end
  end
end
