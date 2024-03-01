defmodule Ash.Test.Actions.TimeoutTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Sleep do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, opts, _) do
      changeset
      |> Ash.Changeset.before_action(fn changeset ->
        :timer.sleep(opts[:ms] || :timer.seconds(1))
        changeset
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        primary? true
        change {Sleep, ms: 1000}
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string, public?: true)
      attribute(:bio, :string, public?: true)
    end
  end

  describe "simple timeout" do
    test "a timeout error is raised" do
      assert_raise Ash.Error.Invalid,
                   ~r/Ash.Test.Actions.TimeoutTest.Author.create timed out after 1ms/,
                   fn ->
                     Domain.create!(Ash.Changeset.for_create(Author, :create, %{name: "Fred"}),
                       timeout: 1
                     )
                   end
    end
  end
end
