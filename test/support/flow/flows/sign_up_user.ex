defmodule Ash.Test.Flow.Flows.SignUpUser do
  @moduledoc false
  use Ash.Flow

  flow do
    domain(Ash.Test.Flow.Domain)

    argument :org_name, :string do
      allow_nil? false
    end

    argument :first_name, :string do
      allow_nil? false
    end

    argument :last_name, :string do
      allow_nil? false
    end

    returns get_org: :org, approve_user: :user
  end

  steps do
    read :get_org, Ash.Test.Flow.Org, :by_name do
      get? true

      input %{
        name: arg(:org_name)
      }
    end

    create :create_user, Ash.Test.Flow.User, :create do
      input %{
        first_name: arg(:first_name),
        last_name: arg(:last_name),
        org: path(result(:get_org), :id)
      }
    end

    update :approve_user, Ash.Test.Flow.User, :approve do
      record result(:create_user)
    end
  end
end
