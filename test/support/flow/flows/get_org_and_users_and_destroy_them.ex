defmodule Ash.Test.Flow.Flows.GetOrgAndUsersAndDestroyThem do
  @moduledoc false
  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :org_name, :string do
      allow_nil? false
    end

    returns :unapprove_users
  end

  steps do
    run_flow :get_org_and_users, Ash.Test.Flow.Flows.GetOrgAndUsers do
      input %{
        org_name: arg(:org_name)
      }
    end

    map :unapprove_users, path(result(:get_org_and_users), :users) do
      destroy :destroy_user, Ash.Test.Flow.User, :destroy do
        record element(:unapprove_users)
      end
    end
  end
end
