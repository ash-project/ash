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

  defmodule Resource do
    use Ash.Resource,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer],
      data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :first_name, :string
      attribute :last_name, :string

      attribute :show_last_name, :boolean, public?: true
    end

    code_interface do
      domain Domain

      define :create
      define :read
    end

    calculations do
      calculate :name, :string, Name, public?: true
      calculate :self_name, :string, SelfName, public?: true
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

      field_policy :name do
        authorize_if always()
      end

      field_policy :self_name do
        authorize_if always()
      end
    end

    actions do
      defaults [:read, create: [:first_name, :last_name, :show_last_name]]
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

  # TODO
  # test "can calculate even if calculate depends on forbidden field from a relationship" do
  #   Resource.create!(%{first_name: "Homer", last_name: "Simpson", show_last_name: false})

  #   [%{name: "Homer", self: %{show_last_name: %Ash.ForbiddenField{}}}] =
  #     Resource.read!(%{}, load: [:self_name, self: [:show_last_name]], authorize?: true)
  # end
end
