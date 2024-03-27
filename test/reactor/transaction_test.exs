defmodule Ash.Test.Reactor.TransactionTest do
  @moduledoc false
  use ExUnit.Case, async: false
  use Mimic

  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia, domain: Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:destroy, create: :*]
    end
  end

  setup do
    ExUnit.CaptureLog.capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain, [Post])
    end)

    on_exit(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  test "when the transaction completes successfully it returns the last result" do
    defmodule SuccessfulNoReturnTransactionReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_domain(Domain)
      end

      transaction :create_posts, Post do
        create :post_1, Post, :create do
          inputs(%{title: value("About Marty McFly")})
        end

        create :post_2, Post, :create do
          inputs(%{title: value("About Doc Brown")})
        end
      end
    end

    assert {:ok, %{title: "About Doc Brown"}} = Reactor.run(SuccessfulNoReturnTransactionReactor)
  end

  test "when the transaction completes successfully it returns the specified result" do
    defmodule SuccessfulNamedReturnTransactionReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_domain(Domain)
      end

      transaction :create_posts, Post do
        create :post_1, Post, :create do
          inputs(%{title: value("About Marty McFly")})
        end

        create :post_2, Post, :create do
          inputs(%{title: value("About Doc Brown")})
        end

        return :post_1
      end
    end

    assert {:ok, %{title: "About Marty McFly"}} =
             Reactor.run(SuccessfulNamedReturnTransactionReactor)
  end

  test "when the transaction fails it is rolled back" do
    defmodule FailAndRollBackTransactionReactor do
      @moduledoc false
      use Ash.Reactor

      ash do
        default_domain(Domain)
      end

      transaction :create_posts, Post do
        create :post_1, Post, :create do
          inputs(%{title: value("About Marty McFly")})
        end

        step :fail do
          run fn _, _ ->
            raise "hell"
          end
        end

        return :post_1
      end
    end

    Ash.DataLayer
    |> expect(:rollback, fn resources, reason ->
      assert resources == [Post]
      assert Exception.message(reason) == "hell"

      raise reason
    end)

    FailAndRollBackTransactionReactor
    |> Reactor.run(%{}, %{}, async?: false)
    |> Ash.Test.assert_has_error(fn
      %Reactor.Error.Invalid.RunStepError{error: error} ->
        Exception.message(error) =~ "hell"

      _ ->
        false
    end)
  end
end
