defmodule Ash.Test.Actions.CreateReactorTest do
  @moduledoc false
  use ExUnit.Case

  defmodule Post do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      allow_unregistered? true
    end
  end

  test "stuff happens" do
    changeset =
      Post
      |> Ash.Changeset.for_create(:create, %{})
      |> Map.put(:api, Api)

    # Reactor.run(return_step: :add_process_context)
  end
end
