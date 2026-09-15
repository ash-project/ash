# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorChangeTest do
  @moduledoc false
  use ExUnit.Case, async: false

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      defaults [:read, :destroy, update: :*, create: :*]
    end
  end

  test "it can create a post" do
    defmodule SimpleCreatePostReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :title

      change :set_title, set_attribute(:title, arg(:title)) do
        initial(Post)
        argument :title, input(:title)
      end

      create :create_post, Post, :create do
        initial(result(:set_title))
      end
    end

    assert {:ok, post} = Reactor.run(SimpleCreatePostReactor, %{title: "Title"})
    assert is_binary(post.id)
    assert post.title == "Title"
  end

  test "it honours the `fail_if_invalid?` option" do
    defmodule SimpleChangeFailReactor do
      @moduledoc false
      use Ash.Reactor

      input :title

      step :changeset do
        run fn _ ->
          {:ok, Ash.Changeset.for_action(Post, :create)}
        end
      end

      change :set_title, set_attribute(:title, arg(:title)) do
        initial(result(:changeset))
        argument :title, input(:title)
        fail_if_invalid?(true)
      end
    end

    assert {:error, invalid} = Reactor.run(SimpleChangeFailReactor, %{title: nil})
    assert Exception.message(invalid) =~ "valid?: false"
  end

  test "it honours the `only_when_valid?` option" do
    defmodule SimpleOnlyWhenValidReactor do
      @moduledoc false
      use Ash.Reactor

      input :changeset
      input :title

      change :set_title, set_attribute(:title, arg(:title)) do
        initial(input(:changeset))
        only_when_valid? true
      end
    end

    valid_changeset = Ash.Changeset.new(Post)

    {:ok, changeset} =
      Reactor.run(SimpleOnlyWhenValidReactor, %{changeset: valid_changeset, title: "Title"})

    assert Ash.Changeset.changing_attribute?(changeset, :title)

    invalid_changeset = %{valid_changeset | valid?: false}

    {:ok, changeset} =
      Reactor.run(SimpleOnlyWhenValidReactor, %{changeset: invalid_changeset, title: "Title"})

    refute Ash.Changeset.changing_attribute?(changeset, :title)
  end

  test "it honours the `where` option" do
    defmodule SimpleWhereChangeReactor do
      @moduledoc false
      use Ash.Reactor

      input :changeset

      change :set_title, set_attribute(:title, "Default title") do
        initial(input(:changeset))
        where [absent(:title)]
      end
    end

    blank_changeset = Ash.Changeset.for_create(Post, :create, %{})
    {:ok, changeset} = Reactor.run(SimpleWhereChangeReactor, %{changeset: blank_changeset})
    assert Ash.Changeset.get_attribute(changeset, :title) == "Default title"

    with_title_changeset = Ash.Changeset.for_create(Post, :create, %{title: "Explicit title"})
    {:ok, changeset} = Reactor.run(SimpleWhereChangeReactor, %{changeset: with_title_changeset})
    assert Ash.Changeset.get_attribute(changeset, :title) == "Explicit title"
  end

  defmodule ReadsArgumentChange do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.change_attribute(
        changeset,
        :title,
        Ash.Changeset.get_argument(changeset, :title)
      )
    end
  end

  test "step arguments are available to the change but are not added to the action's arguments" do
    defmodule ScopedArgumentsReactor do
      @moduledoc false
      use Ash.Reactor

      input :title

      change :set_title, ReadsArgumentChange do
        initial(Post)
        argument :title, input(:title)
      end
    end

    assert {:ok, changeset} = Reactor.run(ScopedArgumentsReactor, %{title: "Title"})
    assert Ash.Changeset.get_attribute(changeset, :title) == "Title"
    refute Map.has_key?(changeset.arguments, :title)

    # the changeset can still be used for an action without `NoSuchInput` errors
    changeset = Ash.Changeset.for_create(changeset, :create)
    assert changeset.valid?
    assert changeset.errors == []
  end

  test "step arguments do not clobber existing arguments on the initial changeset" do
    defmodule ArgumentPost do
      @moduledoc false
      use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

      ets do
        private? true
      end

      attributes do
        uuid_primary_key :id
        attribute :title, :string, allow_nil?: false, public?: true
      end

      actions do
        defaults [:read, :destroy, update: :*]

        create :create do
          argument :title, :string, allow_nil?: false
          change set_attribute(:title, arg(:title))
        end
      end
    end

    defmodule PreservesArgumentsReactor do
      @moduledoc false
      use Ash.Reactor

      input :changeset
      input :title

      change :set_title, set_attribute(:title, arg(:title)) do
        initial(input(:changeset))
        argument :title, input(:title)
      end
    end

    initial = Ash.Changeset.for_create(ArgumentPost, :create, %{title: "From action"})

    assert {:ok, changeset} =
             Reactor.run(PreservesArgumentsReactor, %{changeset: initial, title: "From step"})

    assert Ash.Changeset.get_attribute(changeset, :title) == "From step"
    assert Ash.Changeset.get_argument(changeset, :title) == "From action"
  end

  test "a change step with arguments can be applied to a changeset that already has an action" do
    defmodule ChangeAfterForActionReactor do
      @moduledoc false
      use Ash.Reactor

      input :changeset
      input :title

      change :set_title, set_attribute(:title, arg(:title)) do
        initial(input(:changeset))
        argument :title, input(:title)
      end
    end

    changeset = Ash.Changeset.for_create(Post, :create, %{title: "Original"})
    assert changeset.valid?

    assert {:ok, changeset} =
             Reactor.run(ChangeAfterForActionReactor, %{changeset: changeset, title: "Title"})

    assert changeset.valid?
    assert Ash.Changeset.get_attribute(changeset, :title) == "Title"
    refute Map.has_key?(changeset.arguments, :title)
    refute Enum.any?(changeset.errors, &match?(%Ash.Error.Invalid.NoSuchInput{}, &1))
  end

  defmodule RaisingWhereGuard do
    @moduledoc false
    use Ash.Resource.Validation

    @impl true
    def validate(_changeset, _opts, _context), do: raise("boom in where guard")
  end

  test "a `where` guard that raises fails the step closed instead of skipping the change" do
    defmodule RaisingWhereReactor do
      @moduledoc false
      use Ash.Reactor

      input :changeset

      change :set_title, set_attribute(:title, "Default title") do
        initial(input(:changeset))
        where [RaisingWhereGuard]
      end
    end

    changeset = Ash.Changeset.for_create(Post, :create, %{})

    assert {:error, _} = Reactor.run(RaisingWhereReactor, %{changeset: changeset})
  end
end
