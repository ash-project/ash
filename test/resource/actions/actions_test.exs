defmodule Ash.Test.Dsl.Resource.Actions.ActionsTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Resource.Info
  import Ash.Test.Helpers

  describe "default actions" do
    test "default actions are added" do
      resource =
        defposts do
          actions do
            default_accept :*
            defaults [:create, :read, :update, :destroy]
          end
        end

      assert Ash.Resource.Info.primary_action!(resource, :read)
      assert Ash.Resource.Info.primary_action!(resource, :create)
      assert Ash.Resource.Info.primary_action!(resource, :update)
      assert Ash.Resource.Info.primary_action!(resource, :destroy)
    end
  end

  describe "default accept" do
    test "all params" do
      resource =
        defposts do
          attributes do
            attribute :first_name, :string do
              public?(true)
            end

            attribute :last_name, :string do
              public?(true)
            end
          end

          actions do
            create :all_params, accept: :*
            create :no_params, accept: []
            create :one_param, accept: [:first_name]
            destroy :destroy
          end
        end

      assert Info.action(resource, :all_params).accept == [:first_name, :last_name]
      assert Info.action(resource, :no_params).accept == []
      assert Info.action(resource, :one_param).accept == [:first_name]
      assert Info.action(resource, :destroy).accept == []
    end

    test "some params" do
      resource =
        defposts do
          attributes do
            attribute :first_name, :string do
              public?(true)
            end

            attribute :last_name, :string do
              public?(true)
            end
          end

          actions do
            default_accept [:last_name]

            create :some_params
            create :no_params, accept: []
            create :all_params, accept: :*
          end
        end

      assert Info.action(resource, :some_params).accept == [:last_name]
      assert Info.action(resource, :no_params).accept == []
      assert Info.action(resource, :all_params).accept == [:first_name, :last_name]
    end

    test "no params" do
      resource =
        defposts do
          attributes do
            attribute :first_name, :string do
              public?(true)
            end

            attribute :last_name, :string do
              public?(true)
            end
          end

          actions do
            default_accept []

            create :default_params
            create :one_param, accept: [:first_name]
            create :all_params, accept: :*
          end
        end

      assert Info.action(resource, :default_params).accept == []
      assert Info.action(resource, :one_param).accept == [:first_name]
      assert Info.action(resource, :all_params).accept == [:first_name, :last_name]
    end
  end

  describe "validations" do
    test "raises if you have multiple primary actions for a type" do
      assert_raise(
        Spark.Error.DslError,
        ~r/only one action per type can be the primary/,
        fn ->
          defposts do
            actions do
              default_accept :*
              create :create, primary?: true
              create :special, primary?: true
            end
          end
        end
      )
    end
  end
end
