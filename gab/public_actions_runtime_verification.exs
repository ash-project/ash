Mix.shell().info("Ash public?/private_action? runtime verification (runtime script)")

defmodule Gab.PublicActionsRuntime.Domain do
  use Ash.Domain
end

defmodule Gab.PublicActionsRuntime.Resource do
  use Ash.Resource,
    domain: Gab.PublicActionsRuntime.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    create :public_create
    create :private_create, public?: false
  end
end

alias Gab.PublicActionsRuntime.Resource, as: Resource
alias Ash.Resource.Info
alias Ash.Policy.Check.PrivateAction

actions = Info.actions(Resource)
public_actions = Info.public_actions(Resource)

public_create = Enum.find(actions, &(&1.name == :public_create))
private_create = Enum.find(actions, &(&1.name == :private_create))

public_action_names = Enum.map(public_actions, & &1.name)
all_action_pairs = Enum.map(actions, &{&1.name, &1.public?})

IO.puts("All actions (name, public?): #{inspect(all_action_pairs)}")
IO.puts("Public actions (names): #{inspect(public_action_names)}")

public_private_action_check =
  PrivateAction.match?(nil, %{action: public_create}, [])

private_private_action_check =
  PrivateAction.match?(nil, %{action: private_create}, [])

IO.puts("PrivateAction.check for :public_create => #{inspect(public_private_action_check)}")
IO.puts("PrivateAction.check for :private_create => #{inspect(private_private_action_check)}")

success? =
  public_create &&
    private_create &&
    public_create.public? &&
    !private_create.public? &&
    :public_create in public_action_names &&
    :private_create not in public_action_names &&
    public_private_action_check == false &&
    private_private_action_check == true

if success? do
  IO.puts("RUNTIME VERIFICATION: PASS")
else
  IO.puts("RUNTIME VERIFICATION: FAIL")
  System.halt(1)
end

