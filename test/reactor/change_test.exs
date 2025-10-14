# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
end
