defmodule Ash.Test.Flow.Flows.BranchingTransactionNoRecord do
  @moduledoc false
  use Ash.Flow

  flow do
    domain(Ash.Test.Flow.Domain)

    argument :name, :string do
      allow_nil? false
    end

    returns [:user_not_found, :get_user]
  end

  steps do
    read :get_user, Ash.Test.Flow.User, :by_name do
      get? true
      not_found_error? false

      input(%{
        name: arg(:name)
      })
    end

    branch :user_not_found, expr(is_nil(^result(:get_user))) do
      transaction :create_org_and_user, Ash.Test.Flow.User do
        create :create_org, Ash.Test.Flow.Org, :create do
          input %{name: "Bat Company"}
        end

        create :create_user, Ash.Test.Flow.User, :create do
          input %{
            first_name: arg(:name),
            last_name: "Man",
            org: path(result(:create_org), [:id])
          }
        end
      end
    end
  end
end
