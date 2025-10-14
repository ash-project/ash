# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.CalculationWithAuthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Name do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [:first_name, :last_name, :show_last_name]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           first_name: first_name,
           show_last_name: true,
           last_name: last_name
         } ->
           "#{first_name} #{last_name}"

         %{
           first_name: first_name,
           show_last_name: false
         } ->
           first_name
       end)}
    end
  end

  defmodule SelfName do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [self: [:first_name, :last_name, :show_last_name]]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           self: %{
             first_name: first_name,
             show_last_name: true,
             last_name: last_name
           }
         } ->
           "#{first_name} #{last_name}"

         %{
           self: %{
             first_name: first_name,
             show_last_name: false
           }
         } ->
           first_name
       end)}
    end
  end

  defmodule SelfSelfName do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [self: [self: [:first_name, :last_name, :show_last_name]]]

    @impl true
    def calculate(records, _, _) do
      {:ok,
       records
       |> Enum.map(fn
         %{
           self: %{
             self: %{
               first_name: first_name,
               show_last_name: true,
               last_name: last_name
             }
           }
         } ->
           "#{first_name} #{last_name}"

         %{
           self: %{
             self: %{
               first_name: first_name,
               show_last_name: false
             }
           }
         } ->
           first_name
       end)}
    end
  end

  defmodule EmbeddedNotAllowed do
    use Ash.Resource.Calculation

    @impl true
    def load(_, _, _), do: [embedded: [:not_allowed]]

    @impl true
    def calculate(records, _, _) do
      Enum.map(records, fn record ->
        record.embedded && record.embedded.not_allowed
      end)
    end
  end

  defmodule Embedded do
    use Ash.Resource,
      data_layer: :embedded,
      authorizers: [Ash.Policy.Authorizer]

    resource do
      require_primary_key? false
    end

    attributes do
      attribute :allowed, :string, public?: true
      attribute :not_allowed, :string, public?: true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :allowed do
        authorize_if always()
      end

      field_policy :not_allowed do
        forbid_if always()
      end
    end
  end

  defmodule Resource do
    use Ash.Resource,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :first_name, :string
      attribute :last_name, :string

      attribute :show_last_name, :boolean, public?: true
      attribute :embedded, Embedded
    end

    code_interface do
      domain Domain

      define :create
      define :read
    end

    calculations do
      calculate :name, :string, Name, public?: true
      calculate :self_name, :string, SelfName, public?: true
      calculate :self_self_name, :string, SelfSelfName, public?: true
      calculate :embedded_not_allowed, :string, EmbeddedNotAllowed, public?: true
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end

    field_policies do
      field_policy :show_last_name do
        forbid_if always()
      end

      field_policy [:name, :self_name, :self_self_name, :embedded_not_allowed] do
        authorize_if always()
      end
    end

    actions do
      defaults [:read, create: [:first_name, :last_name, :show_last_name, :embedded]]
    end

    relationships do
      has_one :self, __MODULE__ do
        source_attribute :id
        destination_attribute :id
      end
    end
  end

  test "can calculate even if calculate depends on forbidden field" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{name: "Homer", show_last_name: %Ash.ForbiddenField{}}] =
      Resource.read!(%{}, load: [:name, :show_last_name], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationship" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_name: "Homer"}] =
      Resource.read!(%{}, load: [:self_name, self: [:show_last_name]], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationsip, and the field will show as forbidden in the final data" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_name: "Homer", self: %{show_last_name: %Ash.ForbiddenField{}}}] =
      Resource.read!(%{}, load: [:self_name, self: [:show_last_name]], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from a relationsip two levels deep, and the field will show as forbidden in the final data" do
    Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

    [%{self_self_name: "Homer", self: %{self: %{show_last_name: %Ash.ForbiddenField{}}}}] =
      Resource.read!(%{},
        load: [:self_self_name, self: [self: [:show_last_name]]],
        authorize?: true
      )
  end

  test "can calculate even if calculate depends on forbidden field from an embedded attribute" do
    Resource.create!(%{
      first_name: "Homer",
      last_name: "Simpson",
      show_last_name: false,
      embedded: %{allowed: "shown", not_allowed: "hidden"}
    })

    [%{embedded_not_allowed: "hidden"}] =
      Resource.read!(%{}, load: [:embedded_not_allowed], authorize?: true)
  end

  test "can calculate even if calculate depends on forbidden field from an embedded field, and the field will show as forbidden in the final data" do
    Resource.create!(%{
      first_name: "Homer",
      last_name: "Simpson",
      show_last_name: false,
      embedded: %{allowed: "shown", not_allowed: "hidden"}
    })

    [%{embedded_not_allowed: "hidden", embedded: %{not_allowed: %Ash.ForbiddenField{}}}] =
      Resource.read!(%{}, load: [:embedded_not_allowed], authorize?: true)
  end
end
