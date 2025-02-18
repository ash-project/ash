defmodule Ash.Test.Actions.GenericActionsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule PassingFredOrGeorge do
    use Ash.Policy.SimpleCheck

    def describe(_), do: "is one of the twins"

    def match?(_, %{action_input: action_input}, _) do
      String.downcase(action_input.arguments.name) in ["fred", "george"]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      action :hello_with_default, :string do
        argument :name, :string, allow_nil?: false, default: "default"

        run(fn input, _context ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end

      action :hello, :string do
        argument :name, :string, allow_nil?: false

        run(fn input, _context ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end

      action :do_nothing do
        run fn _, _ -> :ok end
      end

      action :action_without_return_type do
        run fn _, _ -> {:ok, [:this, :will, :not, :be, :used]} end
      end

      action :action_with_return_type, {:array, :atom} do
        run fn _, _ -> {:ok, [:this, :will, :be, :used]} end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string, allow_nil?: false, public?: true)

      timestamps()
    end

    policies do
      policy action(:hello) do
        authorize_if PassingFredOrGeorge
      end

      policy action(:hello_with_default) do
        authorize_if always()
      end

      policy action(:action_without_return_type) do
        authorize_if always()
      end

      policy action(:action_with_return_type) do
        authorize_if always()
      end
    end
  end

  describe "generic actions can be called" do
    test "generic actions can be run" do
      assert "Hello fred" =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
               |> Ash.run_action!()
    end

    test "generic actions validate their input" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
               |> Ash.run_action()

      assert_raise Ash.Error.Invalid, ~r/Invalid Error/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: %{a: 10}})
        |> Ash.run_action!()
      end
    end

    test "generic actions set default values for arguments" do
      assert "Hello default" ==
               Post
               |> Ash.ActionInput.for_action(:hello_with_default, %{})
               |> Ash.run_action!()
    end

    test "generic actions don't accept unknown keys" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:hello, %{name: "fred", one: 1})
               |> Ash.run_action()

      assert_raise Ash.Error.Invalid, ~r/Invalid Error/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "fred", one: 1})
        |> Ash.run_action!()
      end
    end

    test "generic actions return :ok if they don't have a return type" do
      assert :ok =
               Post
               |> Ash.ActionInput.for_action(:action_without_return_type, %{})
               |> Ash.run_action!()
    end

    test "generic actions return the value if they have a return type" do
      assert [:this, :will, :be, :used] =
               Post
               |> Ash.ActionInput.for_action(:action_with_return_type, %{})
               |> Ash.run_action!()
    end
  end

  describe "authorization" do
    test "generic actions can be authorized" do
      # assert "Hello fred" =
      #          Post
      #          |> Ash.ActionInput.for_action(:hello, %{name: "fred"})
      #          |> Ash.run_action!(authorize?: true)

      assert_raise Ash.Error.Forbidden, ~r/Forbidden/, fn ->
        Post
        |> Ash.ActionInput.for_action(:hello, %{name: "mike"})
        |> Ash.run_action!(authorize?: true)
      end
    end
  end

  describe "generic actions wrapping a reactor" do
    defmodule EchoReactor do
      @moduledoc false
      use Reactor

      input :input

      step :echo do
        argument :echo, input(:input)
        run &Map.fetch(&1, :echo)
      end
    end

    defmodule EchoResource do
      @moduledoc false
      use Ash.Resource, domain: Domain

      actions do
        action :echo, :string do
          argument :input, :string, allow_nil?: false

          run EchoReactor
        end
      end

      code_interface do
        define :echo, args: [:input]
      end
    end

    test "it automatically runs the reactor" do
      assert {:ok, "Marty"} = EchoResource.echo("Marty")
    end
  end
end
