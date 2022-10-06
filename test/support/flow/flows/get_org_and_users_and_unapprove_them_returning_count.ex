defmodule Ash.Test.Flow.Flows.GetOrgAndUsersAndUnapproveThemReturningCount do
  @moduledoc false
  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :org_name, :string do
      allow_nil? false
    end

    returns :count_unapproved_users
  end

  steps do
    run_flow :get_org_and_users_and_unapprove_them,
             Ash.Test.Flow.Flows.GetOrgAndUsersAndUnapproveThem do
      input %{
        org_name: arg(:org_name)
      }
    end

    custom :count_unapproved_users, {Ash.Test.Flow.Steps.CountValue, field: :users} do
      input %{
        users: result(:get_org_and_users_and_unapprove_them)
      }
    end
  end
end
