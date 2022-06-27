defmodule Ash.Test.Actions.TimeoutTest do
  @moduledoc false
  use ExUnit.Case, async: false

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
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        primary? true
        change {Sleep, ms: 1000}
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string)
      attribute(:bio, :string)
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "simple timeout" do
    test "a timeout error is raised" do
      assert_raise Ash.Error.Invalid,
                   ~r/Ash.Test.Actions.TimeoutTest.Author.create timed out after 1ms/,
                   fn ->
                     Api.create!(Ash.Changeset.for_create(Author, :create, %{name: "Fred"}),
                       timeout: 1
                     )
                   end
    end
  end
end
