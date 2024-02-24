defmodule Ash.Test.PlugHelpersTest do
  @moduledoc false
  use ExUnit.Case, async: true
  alias Ash.Changeset
  import Ash.PlugHelpers
  import Plug.Conn

  def build_conn, do: Plug.Test.conn(:get, "/")

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :email, :string
    end

    multitenancy do
      strategy :attribute
      attribute :customer_id
    end

    relationships do
      belongs_to :customer, Customer
    end
  end

  defmodule Customer do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :users, User
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Customer
      resource User
    end
  end

  def build_actor(attrs) do
    attrs =
      attrs
      |> Map.put_new_lazy(:customer_id, fn -> build_tenant(%{name: "Deliver-yesterday"}).id end)

    User
    |> Changeset.for_create(:create, attrs, tenant: attrs.customer_id)
    |> Domain.create!()
  end

  def build_tenant(attrs) do
    Customer
    |> Changeset.for_create(:create, attrs)
    |> Domain.create!()
  end

  doctest Ash.PlugHelpers
end
