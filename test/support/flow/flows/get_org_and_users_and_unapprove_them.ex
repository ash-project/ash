defmodule Ash.Test.Flow.Flows.GetOrgAndUsersAndUnapproveThem do
  @moduledoc false
  use Ash.Flow

  flow do
    domain(Ash.Test.Flow.Domain)

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
      update :unapprove_user, Ash.Test.Flow.User, :unapprove do
        record element(:unapprove_users)
      end
    end
  end
end
