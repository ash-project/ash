defmodule Ash.Test.Dsl.Resource.Actions.ActionsTest do
  @moduledoc false
  use ExUnit.Case

  alias Ash.Resource.Info

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          uuid_primary_key :id

          attribute :first_name, :string
          attribute :last_name, :string
        end

        unquote(body)
      end
    end
  end

  test "default actions are added" do
    defposts do
      actions do
        defaults [:create, :read, :update, :destroy]
      end
    end

    assert Ash.Resource.Info.primary_action!(Post, :read)
    assert Ash.Resource.Info.primary_action!(Post, :create)
    assert Ash.Resource.Info.primary_action!(Post, :update)
    assert Ash.Resource.Info.primary_action!(Post, :destroy)
  end

  describe "default accept" do
    test "all params" do
      defposts do
        actions do
          create :all_params
          create :no_params, accept: []
          create :one_param, accept: [:first_name]
        end
      end

      assert Info.action(Post, :all_params).accept == [:id, :first_name, :last_name]
      assert Info.action(Post, :no_params).accept == []
      assert Info.action(Post, :one_param).accept == [:first_name]
    end

    test "some params" do
      defposts do
        actions do
          default_accept [:last_name]

          create :some_params
          create :no_params, reject: :all
          create :all_params, accept: :all
        end
      end

      assert Info.action(Post, :some_params).accept == [:last_name]
      assert Info.action(Post, :no_params).accept == []
      assert Info.action(Post, :all_params).accept == [:id, :first_name, :last_name]
    end

    test "no params" do
      defposts do
        actions do
          default_accept []

          create :default_params
          create :one_param, accept: [:first_name]
          create :all_params, accept: :all
        end
      end

      assert Info.action(Post, :default_params).accept == []
      assert Info.action(Post, :one_param).accept == [:first_name]
      assert Info.action(Post, :all_params).accept == [:id, :first_name, :last_name]
    end
  end

  describe "validations" do
    test "raises if you have multiple primary actions for a type" do
      assert_raise(
        Ash.Error.Dsl.DslError,
        "[Ash.Test.Dsl.Resource.Actions.ActionsTest.Post]\n actions -> create:\n  Multiple actions of type create configured as `primary?: true`, but only one action per type can be the primary",
        fn ->
          defposts do
            actions do
              create :create, primary?: true
              create :special, primary?: true
            end
          end
        end
      )
    end
  end
end
