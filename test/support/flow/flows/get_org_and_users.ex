defmodule Ash.Test.Flow.Flows.GetOrgAndUsers do
  @moduledoc false
  use Ash.Flow

  flow do
    domain(Ash.Test.Flow.Domain)

    argument :org_name, :string do
      allow_nil? false
    end

    returns get_org: :org, list_users: :users
  end

  steps do
    read :get_org, Ash.Test.Flow.Org, :by_name do
      get? true

      input(%{
        name: arg(:org_name)
      })
    end

    read :list_users, Ash.Test.Flow.User, :for_org do
      input(%{
        org: path(result(:get_org), :id)
      })
    end
  end
end
