# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FieldPolicy.FilterCalculationTest do
  @moduledoc """
  Field policies are documented as replacing forbidden fields with `nil` when they
  are referred to in a filter:

  > When these fields are referred to in filters, they will be replaced with an
  > expression that evaluates to nil.

  This holds for attributes, but not for calculations (or aggregates).
  """
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string, public?: true

      # A plain attribute holding the secret value.
      attribute :secret, :string, public?: true
    end

    calculations do
      # The same value, exposed as a calculation.
      calculate :secret_calc, :string, expr(secret), public?: true
    end

    aggregates do
      count :comment_count, :comments, public?: true
    end

    relationships do
      has_many :comments, Ash.Test.Policy.FieldPolicy.FilterCalculationTest.Comment do
        public? true
      end
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy [:secret, :secret_calc, :comment_count] do
        authorize_if actor_attribute_equals(:admin, true)
      end

      field_policy :* do
        authorize_if always()
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :post, Post, public?: true, allow_nil?: false
    end
  end

  setup do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{title: "post", secret: "shh"})
      |> Ash.create!(authorize?: false)

    Comment
    |> Ash.Changeset.for_create(:create, %{post_id: post.id})
    |> Ash.create!(authorize?: false)

    %{post: post, admin: %{admin: true}, user: %{admin: false}}
  end

  describe "an actor who can see the field" do
    test "can filter on the attribute", %{admin: admin} do
      assert [_] =
               Post
               |> Ash.Query.filter_input(secret: "shh")
               |> Ash.read!(actor: admin, authorize?: true)
    end

    test "can filter on the calculation", %{admin: admin} do
      assert [_] =
               Post
               |> Ash.Query.filter_input(secret_calc: "shh")
               |> Ash.read!(actor: admin, authorize?: true)
    end

    test "can filter on the aggregate", %{admin: admin} do
      assert [_] =
               Post
               |> Ash.Query.filter_input(comment_count: 1)
               |> Ash.read!(actor: admin, authorize?: true)
    end
  end

  describe "an actor who cannot see the field" do
    # This one passes today.
    test "filtering on the attribute sees nil", %{user: user} do
      assert [] =
               Post
               |> Ash.Query.filter_input(secret: "shh")
               |> Ash.read!(actor: user, authorize?: true)

      assert [_] =
               Post
               |> Ash.Query.filter_input(%{secret: %{is_nil: true}})
               |> Ash.read!(actor: user, authorize?: true)
    end

    # This one fails today: the filter is applied against the real value.
    test "filtering on the calculation sees nil", %{user: user} do
      assert [] =
               Post
               |> Ash.Query.filter_input(secret_calc: "shh")
               |> Ash.read!(actor: user, authorize?: true)

      assert [_] =
               Post
               |> Ash.Query.filter_input(%{secret_calc: %{is_nil: true}})
               |> Ash.read!(actor: user, authorize?: true)
    end

    # Same failure for aggregates.
    test "filtering on the aggregate sees nil", %{user: user} do
      assert [] =
               Post
               |> Ash.Query.filter_input(comment_count: 1)
               |> Ash.read!(actor: user, authorize?: true)
    end
  end
end
