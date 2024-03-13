defmodule Ash.Test.ReactorReadOneTest do
  @moduledoc false
  use ExUnit.Case, async: true

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

    assert {:error, [error]} = Reactor.run(SimpleReadOneReactor, %{}, %{}, async?: false)
    assert Exception.message(error) =~ "expected at most one result"
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

    assert {:error, [error]} = Reactor.run(NotFoundReactor, %{}, %{}, async?: false)
    assert Exception.message(error) =~ "not found"
  end

  test "when exactly one record is returned it returns it" do
    expected = Post.create!(%{title: "Marty"})

    assert {:ok, actual} = Reactor.run(SimpleReadOneReactor, %{}, %{}, async?: false)
    assert expected.id == actual.id
  end
end
