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
            attribute :first_name, :string
            attribute :last_name, :string
          end

          actions do
            create :all_params
            create :no_params, accept: []
            create :one_param, accept: [:first_name]
            destroy :destroy
            destroy :destroy_one_param, accept: [:first_name]
          end
        end

      assert Info.action(resource, :all_params).accept == [:first_name, :last_name]
      assert Info.action(resource, :no_params).accept == []
      assert Info.action(resource, :one_param).accept == [:first_name]
      assert Info.action(resource, :destroy).accept == []
      assert Info.action(resource, :destroy_one_param).accept == [:first_name]
    end

    test "some params" do
      resource =
        defposts do
          attributes do
            attribute :first_name, :string
            attribute :last_name, :string
          end

          actions do
            default_accept [:last_name]

            create :some_params
            create :no_params, reject: :all
            create :all_params, accept: :all
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
            attribute :first_name, :string
            attribute :last_name, :string
          end

          actions do
            default_accept []

            create :default_params
            create :one_param, accept: [:first_name]
            create :all_params, accept: :all
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
              create :create, primary?: true
              create :special, primary?: true
            end
          end
        end
      )
    end

    test "raise if accept and reject keys overlap" do
      assert_raise(
        Spark.Error.DslError,
        ~r/accept and reject keys cannot overlap/,
        fn ->
          defposts do
            attributes do
              attribute :attr, :string
            end

            actions do
              create :create_2 do
                accept [:attr, :id]
                reject [:attr]
              end
            end
          end
        end
      )
    end
  end
end
