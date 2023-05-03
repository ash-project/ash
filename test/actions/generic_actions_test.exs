defmodule Ash.Test.Actions.GenericActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule PassingFredOrGeorge do
    use Ash.Policy.SimpleCheck

    def describe(_), do: "is one of the twins"

    def match?(_, %{action_input: action_input}, _) do
      String.downcase(action_input.arguments.name) in ["fred", "george"]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      action :hello, :string do
        argument :name, :string, allow_nil?: false

        run(fn input, _context ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false)

      timestamps()
    end

    policies do
      policy action(:hello) do
        authorize_if PassingFredOrGeorge
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "generic actions can be called" do
    test "generic actions can be run" do
      assert "Hello fred" =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
               |> Api.run_action!()
    end

    test "generic actions validate their input" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
               |> Api.run_action()

      assert_raise Ash.Error.Invalid, ~r/Input Invalid/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
        |> Api.run_action!()
      end
    end
  end

  describe "authorization" do
    test "generic actions can be authorized" do
      assert "Hello fred" =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
               |> Api.run_action!(authorize?: true)

      assert_raise Ash.Error.Forbidden, ~r/Forbidden/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "mike"})
        |> Api.run_action!(authorize?: true)
      end
    end
  end
end
