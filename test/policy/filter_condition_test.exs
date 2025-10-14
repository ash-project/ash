# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FilterConditionTest do
  use ExUnit.Case, async: true

  defmodule Resource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.FilterConditionTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key :id
      attribute :visible, :boolean, allow_nil?: false, public?: true
    end

    policies do
      default_access_type :filter

      policy [action(:read), expr(visible == true)] do
        authorize_if always()
      end
    end
  end

  defmodule RuntimeFalsyCheck do
    @moduledoc false
    use Ash.Policy.Check

    @impl Ash.Policy.Check
    def describe(_), do: "returns false at runtime"

    @impl Ash.Policy.Check
    def strict_check(_, _, _), do: {:ok, :unknown}

    @impl Ash.Policy.Check
    def check(_actor, _list, _map, _options), do: []
  end

  defmodule FilterFalsyCheck do
    @moduledoc false
    use Ash.Policy.Check

    @impl Ash.Policy.Check
    def describe(_), do: "returns false in a filter"

    @impl Ash.Policy.Check
    def strict_check(_, _, _), do: {:ok, false}
  end

  defmodule FinalBypassResource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.FilterConditionTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      bypass always() do
        access_type :runtime
        description "Bypass never active"
        authorize_if FilterFalsyCheck
      end
    end
  end

  defmodule RuntimeBypassResource do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Policy.FilterConditionTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults([:read, :destroy, create: :*, update: :*])
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      default_access_type :filter

      bypass RuntimeFalsyCheck do
        description "Bypass never active"
        authorize_if always()
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    authorization do
      authorize :by_default
    end

    resources do
      resource Resource
      resource RuntimeBypassResource
      resource FinalBypassResource
    end
  end

  test "condition in filter policy is evaluated" do
    Resource
    |> Ash.Changeset.for_create(:create, %{visible: true}, authorize?: false)
    |> Ash.create!()

    Resource
    |> Ash.Changeset.for_create(:create, %{visible: false}, authorize?: false)
    |> Ash.create!()

    [visible_resource] =
      Resource
      |> Ash.Query.for_read(:read, %{}, actor: %{id: "foo"})
      |> Ash.read!()

    assert visible_resource.visible == true
  end

  test "can use other resource's calculation in expr" do
    defmodule Author do
      @moduledoc false
      use Ash.Resource,
        domain: Ash.Test.Policy.FilterConditionTest.Diary,
        data_layer: Ash.DataLayer.Ets,
        authorizers: [Ash.Policy.Authorizer]

      ets do
        private?(true)
      end

      actions do
        default_accept :*
        defaults([:read, :destroy, create: :*, update: :*])
      end

      attributes do
        uuid_primary_key :id
        attribute :is_active, :boolean, public?: true
      end

      calculations do
        calculate :is_deactivated, :boolean, expr(not is_active)
      end

      policies do
        policy action_type(:create) do
          authorize_if always()
        end

        policy action_type(:read) do
          authorize_if always()
        end

        policy action_type(:update) do
          authorize_if expr(id == ^actor(:id))
        end
      end
    end

    defmodule Post do
      @moduledoc false
      use Ash.Resource,
        domain: Ash.Test.Policy.FilterConditionTest.Diary,
        data_layer: Ash.DataLayer.Ets,
        authorizers: [Ash.Policy.Authorizer]

      ets do
        private?(true)
      end

      actions do
        default_accept :*
        defaults([:read, :destroy, create: :*, update: :*])
      end

      attributes do
        uuid_primary_key :id
        attribute :title, :string, public?: true
        attribute :is_active, :boolean, default: true, public?: true
      end

      relationships do
        belongs_to :author, Author, public?: true
      end

      changes do
        change relate_actor(:author)
      end

      policies do
        policy action_type(:create) do
          authorize_if always()
        end

        policy action_type(:read) do
          authorize_if always()
        end

        policy action_type(:update) do
          forbid_if expr(author.is_deactivated)
          authorize_if always()
        end
      end
    end

    defmodule Diary do
      @moduledoc false
      use Ash.Domain

      authorization do
        authorize :by_default
      end

      resources do
        resource Author
        resource Post
      end
    end

    assert author =
             Author
             |> Ash.Changeset.for_create(:create, %{is_active: false})
             |> Ash.create!()

    assert post =
             Post
             |> Ash.Changeset.for_create(:create, %{title: "title 1"}, actor: author)
             |> Ash.create!()

    assert {:error, %Ash.Error.Forbidden{}} =
             post
             |> Ash.Changeset.for_update(:update, %{title: "title 2"}, actor: author)
             |> Ash.update()
  end

  test "bypass works with filter policies" do
    RuntimeBypassResource
    |> Ash.Changeset.for_create(:create, %{}, authorize?: false)
    |> Ash.create!()

    assert [] =
             RuntimeBypassResource
             |> Ash.Query.for_read(:read, %{}, actor: nil, authorize?: true)
             |> Ash.read!()
  end

  test "bypass at the end works with filter policies" do
    FinalBypassResource
    |> Ash.Changeset.for_create(:create, %{}, authorize?: false)
    |> Ash.create!()

    assert [] =
             FinalBypassResource
             |> Ash.Query.for_read(:read, %{}, actor: nil, authorize?: true)
             |> Ash.read!()
  end
end
