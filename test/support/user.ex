defmodule Ash.Test.User do
  @moduledoc false
  use Ash.Resource, data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  code_interface do
    define_for Ash.Test.Api
    define :get_user, action: :read, get?: true, args: [:id]
    define :get_user_safely, action: :read, get?: true, args: [:id], not_found_error?: false
    define :read_users, action: :read
    define :get_by_id, action: :read, get_by: [:id]
    define :create, args: [{:optional, :first_name}]
    define :welcome, args: [:greeting, :name]

    define_calculation(:full_name, args: [:first_name, :last_name])

    define_calculation(:full_name_opt,
      calculation: :full_name,
      args: [:first_name, :last_name, {:optional, :separator}]
    )

    define_calculation(:full_name_record, calculation: :full_name, args: [:_record])
  end

  actions do
    read :read do
      primary? true
    end

    create :create

    read :by_id do
      argument :id, :uuid, allow_nil?: false

      filter expr(id == ^arg(:id))
    end

    action :welcome, :string do
      argument :greeting, :string do
        allow_nil? false
      end

      argument :name, :string do
        allow_nil? false
        description "The name of the person to be greeted"
      end

      run(fn input, _ ->
        {:ok, "#{input.arguments.greeting}, #{input.arguments.name}!"}
      end)
    end
  end

  calculations do
    calculate :full_name, :string, expr(first_name <> ^arg(:separator) <> last_name) do
      argument :separator, :string, default: " ", allow_nil?: false
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :first_name, :string do
      default "fred"
    end

    attribute :last_name, :string do
      description "The user's last name"
    end
  end
end

defmodule Ash.Test.Registry do
  @moduledoc false
  use Ash.Registry

  entries do
    entry(Ash.Test.User)
  end
end

defmodule Ash.Test.Api do
  @moduledoc false
  use Ash.Api

  resources do
    registry Ash.Test.Registry
  end
end
