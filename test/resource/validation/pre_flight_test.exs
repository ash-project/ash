# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Validation.PreFlightTest do
  use ExUnit.Case, async: true
  alias Ash.Test.Domain, as: Domain

  defmodule AlwaysRaises do
    use Ash.Resource.Validation

    @impl true
    def init(opts), do: {:ok, opts}

    @impl true
    def validate(_changeset, _opts, _context) do
      raise "validation was not skipped"
    end

    @impl true
    def atomic(_changeset, _opts, _context), do: raise("validation was not skipped")

    @impl true
    def describe(_opts), do: [message: "always raises", vars: %{}]
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:read, :create]

      update :skip_during_pre_flight_authorization do
        validate AlwaysRaises do
          where [negate(pre_flight_authorization())]
        end
      end

      update :only_during_pre_flight_authorization do
        validate AlwaysRaises do
          where [pre_flight_authorization()]
        end
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    attributes do
      uuid_v7_primary_key :id
      attribute :title, :string, allow_nil?: true, public?: true
    end
  end

  describe "negate(pre_flight_authorization())" do
    test "skips validation during Ash.can? (pre_flight?: true)" do
      post = Ash.create!(Post, %{title: "hello"})
      assert Ash.can?({post, :skip_during_pre_flight_authorization}, nil, pre_flight?: true)
    end

    test "runs validation during Ash.can? with pre_flight?: false" do
      post = Ash.create!(Post, %{title: "hello"})

      assert_raise Ash.Error.Unknown, ~r/validation was not skipped/, fn ->
        Ash.can?({post, :skip_during_pre_flight_authorization}, nil, pre_flight?: false)
      end
    end

    test "runs validation during normal action execution" do
      post = Ash.create!(Post, %{title: "hello"})

      assert_raise Ash.Error.Unknown, ~r/validation was not skipped/, fn ->
        Ash.update(post, %{}, action: :skip_during_pre_flight_authorization)
      end
    end
  end

  describe "pre_flight_authorization()" do
    test "runs validation during Ash.can? (pre_flight?: true)" do
      post = Ash.create!(Post, %{title: "hello"})

      assert_raise Ash.Error.Unknown, ~r/validation was not skipped/, fn ->
        Ash.can?({post, :only_during_pre_flight_authorization}, nil, pre_flight?: true)
      end
    end

    test "skips validation during Ash.can? with pre_flight?: false" do
      post = Ash.create!(Post, %{title: "hello"})
      assert Ash.can?({post, :only_during_pre_flight_authorization}, nil, pre_flight?: false)
    end

    test "skips validation during normal action execution" do
      post = Ash.create!(Post, %{title: "hello"})
      assert {:ok, _} = Ash.update(post, %{}, action: :only_during_pre_flight_authorization)
    end
  end
end
