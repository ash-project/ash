defmodule Ash.Test.Flow.Flows.BranchingTransaction do
  @moduledoc false
  use Ash.Flow

  flow do
    domain(Ash.Test.Flow.Domain)

    argument :do_branch, :boolean do
      allow_nil? false
    end

    argument :name, :string do
      allow_nil? false
    end

    returns :change_user_email
  end

  steps do
    branch :branch, arg(:do_branch) do
      read :get_user, Ash.Test.Flow.User, :by_name do
        get? true

        input(%{
          name: arg(:name)
        })
      end

      transaction :multi_user_update, Ash.Test.Flow.User do
        update :unapprove_user, Ash.Test.Flow.User, :unapprove do
          record result(:get_user)
        end

        update :change_user_email, Ash.Test.Flow.User, :update do
          record result(:unapprove_user)
          input %{email: "changed@example.com"}
        end
      end
    end
  end
end
