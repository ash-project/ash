defmodule Ash.Test.Flow.Flows.GetUserNotFoundError do
  @moduledoc false
  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :user_name, :string do
      allow_nil? false
    end

    returns :get_user
  end

  steps do
    read :get_user, Ash.Test.Flow.User, :by_name do
      get? true
      not_found_error? false

      input(%{
        name: arg(:user_name)
      })
    end
  end
end
