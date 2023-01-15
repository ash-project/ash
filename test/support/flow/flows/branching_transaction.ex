defmodule Ash.Test.Flow.Flows.BranchingTransaction do
  @moduledoc false
  use Ash.Flow

  flow do
    argument :do_branch, :boolean do
      allow_nil? false
      default true
    end

    argument :name, :string do
      allow_nil? false
    end

    returns :branch
  end

  steps do
    branch :branch, arg(:do_branch) do
      read :get_user, Ash.Test.Flow.User, :by_name do
        input(%{
          name: arg(:name)
        })
      end

      transaction :multi_user_update, Ash.Test.Flow.User do
        update :approve_user, Ash.Test.Flow.User, :approve do
          record result(:get_user)
        end

        update :change_user_email, Ash.Test.Flow.User, :update do
          record result(:get_user)
          input %{email: "changed@example.com"}
        end
      end
    end
  end
end
