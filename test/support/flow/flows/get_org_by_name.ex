defmodule Ash.Test.Flow.Flows.GetOrgByName do
  @moduledoc false
  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :org_name, :string do
      allow_nil? false
    end

    returns :get_org
  end

  steps do
    read :get_org, Ash.Test.Flow.Org, :by_name do
      get? true

      input(%{
        name: arg(:org_name)
      })
    end
  end
end
