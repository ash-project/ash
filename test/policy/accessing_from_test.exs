defmodule Ash.Test.Policy.AccessingFromTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Support.PolicyComplex.{Team, User}

  require Logger

  describe "many_to_many relationship" do
    test "creates record via accessing_from" do
      team = Ash.create!(Team, %{name: "Sports"}, authorize?: false)
      team_id = team.id

      assert {:ok, %{teams: [%{id: ^team_id}]}} =
               User
               |> Ash.Changeset.for_create(:with_teams, %{name: "Name", teams: [team_id]})
               |> Ash.create(load: [:teams])
    end
  end
end
