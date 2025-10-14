# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorReadOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias __MODULE__, as: Self
  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    relationships do
      has_many :comments, Self.Comment, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    code_interface do
      define :create
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :comment, :string, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :post, Self.Post, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    code_interface do
      define :create
    end
  end

  defmodule SimpleReadOneReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    ash do
      default_domain(Domain)
    end

    read_one(:read_one_post, Ash.Test.ReactorReadOneTest.Post, :read)
  end

  test "when more than one record is returned it returns an error" do
    ~w[Marty Doc Einstein]
    |> Enum.each(&Post.create!(%{title: &1}))

    SimpleReadOneReactor
    |> Reactor.run(%{}, %{}, async?: false)
    |> Ash.Test.assert_has_error(fn
      %Reactor.Error.Invalid.RunStepError{error: error} ->
        Exception.message(error) =~ "expected at most one result"

      _ ->
        false
    end)
  end

  test "when no records are returned it returns nil" do
    assert {:ok, nil} = Reactor.run(SimpleReadOneReactor, %{}, %{}, async?: false)
  end

  test "when no records are returned it can return an error" do
    defmodule NotFoundReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      read_one :read_one_post, Ash.Test.ReactorReadOneTest.Post, :read do
        fail_on_not_found?(true)
      end
    end

    NotFoundReactor
    |> Reactor.run(%{}, %{}, async?: false)
    |> Ash.Test.assert_has_error(fn
      %Reactor.Error.Invalid.RunStepError{
        error: %Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}
      } ->
        true

      _ ->
        false
    end)
  end

  test "when exactly one record is returned it returns it" do
    expected = Post.create!(%{title: "Marty"})

    assert {:ok, actual} = Reactor.run(SimpleReadOneReactor, %{}, %{}, async?: false)
    assert expected.id == actual.id
  end

  test "it can load related data when asked" do
    defmodule LoadRelatedReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      read_one :read_one_post, Ash.Test.ReactorReadOneTest.Post, :read do
        load value(comments: [:comment])
      end
    end

    post = Post.create!(%{title: "Marty"})
    comments = ["This is heavy", "You made a time machine... out of a Delorean?"]

    for comment <- comments do
      Comment.create!(%{post_id: post.id, comment: comment})
    end

    assert {:ok, post} = Reactor.run(LoadRelatedReactor, %{}, %{}, async?: false)
    assert Enum.sort(Enum.map(post.comments, & &1.comment)) == comments
  end
end
