defmodule Ash.Test.Flow.Flows.UnapproveAllUsers do
  @moduledoc "Foo"
  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :org_name, :string do
      allow_nil? false
    end

    argument :error, :atom do
      constraints one_of: [:raise, :return]
    end

    returns :count_unapproved_users
  end

  steps do
    read :get_org, Ash.Test.Flow.Org, :by_name do
      get? true

      input(%{
        name: arg(:org_name)
      })
    end

    transaction :get_org_and_unapprove_users, Ash.Test.Flow.Org do
      read :list_users, Ash.Test.Flow.User, :for_org do
        input %{
          org: path(result(:get_org), :id)
        }
      end

      map :unapprove_users, result(:list_users) do
        update :unapprove_user, Ash.Test.Flow.User, :unapprove do
          record element(:unapprove_users)
        end
      end

      custom :count_unapproved_users, {Ash.Test.Flow.Steps.CountValue, field: :users} do
        input %{
          users: result(:list_users)
        }
      end

      custom :error, Ash.Test.Flow.Steps.Error do
        input %{
          error: arg(:error)
        }
      end
    end
  end
end
