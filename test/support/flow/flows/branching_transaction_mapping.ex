defmodule Ash.Test.Flow.Flows.BranchingTransactionMapping do
  @moduledoc false
  use Ash.Flow

  alias Ash.Test.Flow.{Api, Org, User}

  flow do
    api Api

    argument :org_name, :string do
      allow_nil? false
    end

    argument :user_attributes, :term do
      allow_nil? false
    end

    argument :update_users, :boolean do
      default nil
    end

    returns :create_or_update_users
  end

  steps do
    transaction :create_or_update_users, User do
      read :get_org, Org, :by_name do
        input %{
          name: arg(:org_name)
        }
      end

      map :cycle_users, arg(:user_attributes) do
        branch :creation_bulk, expr(is_nil(^arg(:update_users))) do
          custom :create_user, Ash.Test.Flow.Steps.CreateUser do
            input %{
              attributes: element(:cycle_users),
              org: path(result(:get_org), :id)
            }
          end
        end

        branch :update_bulk, expr(not is_nil(^arg(:update_users))) do
          custom :update_user, Ash.Test.Flow.Steps.UpdateUser do
            input %{
              attributes: element(:cycle_users)
            }
          end
        end
      end
    end
  end
end

defmodule MmsBiztalk.Flows.Steps.CreateUser do
  use Ash.Flow.Step
  alias Ash.Changeset

  def run(input, _opts, _context) do
    created_user =
      Ash.Test.Flow.User
      |> Changeset.for_create(:create, input.attributes |> Map.merge(%{org: input.org}))
      |> Ash.Test.Flow.Api.create()

    case created_user do
      {:ok, resource_record} ->
        resource_record |> Ash.Test.Flow.User.to_approved()

      {:error, _error} ->
        {:ok, nil}
    end
  end
end

defmodule MmsBiztalk.Flows.Steps.UpdateUser do
  use Ash.Flow.Step
  alias Ash.Changeset

  def run(input, _opts, _context) do
    user_to_update =
      Ash.Test.Flow.User
      |> Ash.Query.for_read(
        :by_name,
        %{name: input.attributes.firstname}
      )
      |> Ash.Test.Flow.Api.read!()

    user_updated =
      user_to_update
      |> Changeset.for_update(:update, input.attributes)
      |> Ash.Test.Flow.Api.update()

    case user_updated do
      {:ok, resource_record} ->
        resource_record |> Ash.Test.Flow.User.to_approved()

      {:error, _error} ->
        {:ok, nil}
    end
  end
end
