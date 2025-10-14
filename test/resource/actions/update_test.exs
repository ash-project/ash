# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Dsl.Resource.Actions.UpdateTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource,
          domain: Domain,
          data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "it creates an action" do
      defposts do
        actions do
          default_accept :*
          update :update
        end
      end

      assert [
               %Ash.Resource.Actions.Update{
                 name: :update,
                 primary?: false,
                 type: :update
               }
             ] = Ash.Resource.Info.actions(Post)
    end
  end

  describe "validation" do
    test "it fails if `name` is not an atom" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :name option: expected atom, got: "default"/,
        fn ->
          defposts do
            actions do
              default_accept :*
              update "default"
            end
          end
        end
      )
    end

    test "it fails if `primary?` is not a boolean" do
      assert_raise(
        Spark.Error.DslError,
        ~r/invalid value for :primary\? option: expected boolean, got: 10/,
        fn ->
          defposts do
            actions do
              default_accept :*
              update :update, primary?: 10
            end
          end
        end
      )
    end
  end

  describe "updates with argument-based changes" do
    test "atomic_update should work with argument expressions without requiring attribute in accept list" do
      defmodule Post1 do
        @moduledoc false
        use Ash.Resource,
          domain: Domain,
          data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :oban_job_id, :integer
        end

        actions do
          defaults [:read, create: []]

          update :set_oban_job_id do
            argument :job_id, :integer, allow_nil?: false

            change atomic_update(:oban_job_id, expr(^arg(:job_id)))
          end
        end

        code_interface do
          define :create, action: :create
          define :set_oban_job_id, action: :set_oban_job_id, args: [:job_id]
        end
      end

      {:ok, post} = Post1.create()

      updated_post = Post1.set_oban_job_id!(post, 123)
      assert updated_post.oban_job_id == 123
    end

    test "set_attribute should work with argument expressions without requiring attribute in accept list" do
      defmodule Post2 do
        @moduledoc false
        use Ash.Resource,
          domain: Domain,
          data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
          attribute :oban_job_id, :integer
        end

        actions do
          defaults [:read, create: []]

          update :set_oban_job_id do
            argument :job_id, :integer, allow_nil?: false

            change set_attribute(:oban_job_id, arg(:job_id))
          end
        end

        code_interface do
          define :create, action: :create
          define :set_oban_job_id, action: :set_oban_job_id, args: [:job_id]
        end
      end

      {:ok, post} = Post2.create()

      assert {:ok, updated_post} = Post2.set_oban_job_id(post, 123)
      assert updated_post.oban_job_id == 123
    end
  end
end
