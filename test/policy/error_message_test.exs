# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.ErrorMessageTest do
  @moduledoc false
  use ExUnit.Case, async: true
  require Ash.Query

  defmodule CustomForbidden do
    use Splode.Error, fields: [:where], class: :forbidden

    def message(error), do: "custom forbidden in #{inspect(error.where)}"
  end

  defmodule Post do
    @moduledoc """
    Uses create actions for the test cases because reads with filter-mode
    policies filter to `[]` rather than raising — the custom-message path
    only fires when a policy emits a forbidden error.
    """
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :text, :string, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy]

      create :requires_admin
      create :explicit_forbid
      create :dynamic_string
      create :returns_exception
      create :multi_policy_prefers_forbid
      create :no_message
    end

    policies do
      policy action(:requires_admin) do
        error_message("only admins can do this")
        authorize_if actor_attribute_equals(:is_admin, true)
      end

      policy action(:explicit_forbid) do
        error_message("explicit deny")
        forbid_if actor_attribute_equals(:banned, true)
        authorize_if always()
      end

      policy action(:dynamic_string) do
        error_message(fn _subject, context ->
          "actor #{inspect(context.actor)} can't do that"
        end)

        authorize_if actor_attribute_equals(:is_admin, true)
      end

      policy action(:returns_exception) do
        error_message(fn _subject, context ->
          CustomForbidden.exception(where: context.action.name)
        end)

        authorize_if actor_attribute_equals(:is_admin, true)
      end

      # First policy doesn't authorize (state :unknown);
      # second policy explicitly forbids (state :forbidden).
      # Responsible policy should be the explicit-forbid one.
      policy action(:multi_policy_prefers_forbid) do
        error_message("first: needs admin")
        authorize_if actor_attribute_equals(:is_admin, true)
      end

      policy action(:multi_policy_prefers_forbid) do
        error_message("second: explicit deny")
        forbid_if actor_attribute_equals(:banned, true)
        authorize_if always()
      end

      # No error_message — should fall back to default forbidden text.
      policy action(:no_message) do
        authorize_if actor_attribute_equals(:is_admin, true)
      end

      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:destroy) do
        authorize_if always()
      end
    end
  end

  defp create(action_name, actor) do
    Post
    |> Ash.Changeset.for_create(action_name, %{text: "hi"})
    |> Ash.create(actor: actor)
  end

  describe "static error_message string" do
    test "surfaces on a policy whose checks fail to authorize" do
      assert {:error, %Ash.Error.Forbidden{} = error} =
               create(:requires_admin, %{is_admin: false})

      assert Exception.message(error) =~ "only admins can do this"
    end

    test "surfaces on a policy whose forbid_if fires" do
      assert {:error, %Ash.Error.Forbidden{} = error} =
               create(:explicit_forbid, %{banned: true})

      assert Exception.message(error) =~ "explicit deny"
    end
  end

  describe "dynamic error_message function returning a string" do
    test "receives subject + context and the returned string is surfaced" do
      assert {:error, %Ash.Error.Forbidden{} = error} =
               create(:dynamic_string, %{id: "u1", is_admin: false})

      message = Exception.message(error)
      assert message =~ "can't do that"
      assert message =~ "u1"
    end
  end

  describe "dynamic error_message function returning an exception" do
    test "replaces the default Forbidden.Policy exception entirely" do
      assert {:error, %Ash.Error.Forbidden{errors: errors}} =
               create(:returns_exception, %{is_admin: false})

      assert Enum.any?(errors, &match?(%CustomForbidden{}, &1))
      refute Enum.any?(errors, &match?(%Ash.Error.Forbidden.Policy{}, &1))

      assert Exception.message(Enum.find(errors, &match?(%CustomForbidden{}, &1))) =~
               "custom forbidden in :returns_exception"
    end
  end

  describe "multi-policy responsibility" do
    test "first responsible policy's message wins when neither has an explicit-forbid fact computed" do
      # The SAT solver doesn't compute facts it doesn't need. Once the first
      # policy fails to authorize, the request is forbidden regardless of the
      # second policy's outcome, so the second policy's facts may be unknown.
      # With pure-fact resolution, both policies look :unknown and the first
      # in source order wins.
      assert {:error, %Ash.Error.Forbidden{} = error} =
               create(:multi_policy_prefers_forbid, %{is_admin: false, banned: false})

      assert Exception.message(error) =~ "first: needs admin"
    end
  end

  describe "without error_message" do
    test "falls back to the default forbidden text" do
      assert {:error, %Ash.Error.Forbidden{} = error} =
               create(:no_message, %{is_admin: false})

      assert Exception.message(error) =~ "forbidden"
    end
  end
end
