# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.BuilderTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Builder

  defmodule Change do
    use Ash.Resource.Change

    @impl true
    def init(opts) do
      {:ok, opts}
    end

    @impl true
    def change(%Ash.Changeset{} = changeset, _opts, _context) do
      changeset
    end
  end

  defmodule Validation do
    use Ash.Resource.Validation

    @impl true
    def init(opts) do
      {:ok, opts}
    end

    @impl true
    def validate(%Ash.Changeset{} = _changeset, _opts, _context) do
      :ok
    end
  end

  describe "resource builder" do
    test "build action change" do
      assert {:ok, %Ash.Resource.Change{}} =
               Builder.build_action_change({Ash.Test.Resource.BuilderTest.Change, []})
    end

    test "build action validation" do
      assert {:ok, %Ash.Resource.Validation{}} =
               Builder.build_action_validation({Ash.Test.Resource.BuilderTest.Change, []})
    end
  end
end
