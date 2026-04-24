# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.DomainTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Domain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Foo do
    use Ash.Resource, domain: Domain

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  test "double macros invocation `use Ash.Domain` should raises a CompileError" do
    code = """
    defmodule DoubleMacrosDomain do
      use Ash.Domain
      use Ash.Domain, extensions: []
    end
    """

    try do
      Code.compile_string(code)
      flunk("expected CompileError on the second `use Ash.Domain`")
    rescue
      e in CompileError ->
        # Error should point to line 3 (the second `use Ash.Domain` invocation)
        assert e.line == 3
        assert String.contains?(e.description, "use Ash.Domain")
        assert String.contains?(e.description, "can only be called once")
        assert String.contains?(e.description, "Remove the duplicate")
    end
  end

  test "single macros invocation `use Ash.Domain` should compile" do
    code = """
    defmodule SingleMacrosDomain do
      use Ash.Domain
    end
    """

    assert _compiled = Code.compile_string(code)
  end

  test "cannot define a resource that points to a domain that doesn't accept it" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule NoResourcesDomain do
          use Ash.Domain
        end

        defmodule Bar do
          use Ash.Resource, domain: NoResourcesDomain

          attributes do
            uuid_primary_key :id
          end
        end
      end)

    assert String.contains?(output, "domain does not accept this resource")
  end

  test "cannot define a code interface to a non-existing action" do
    assert_raise Spark.Error.DslError, ~r/refers to a non-existent action/, fn ->
      defmodule Baz do
        use Ash.Resource, domain: BazDomain

        attributes do
          uuid_primary_key :id
        end
      end

      defmodule BazDomain do
        use Ash.Domain

        resources do
          resource Baz do
            define :hello, args: [:name, :bar]
          end
        end
      end
    end
  end

  test "cannot define a code interface with invalid arguments" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule FooBar do
          use Ash.Resource, domain: FooBarDomain

          attributes do
            uuid_primary_key :id
          end

          actions do
            action :hello, :string do
              argument :name, :string, allow_nil?: false

              run(fn input, _context ->
                {:ok, "Hello #{input.arguments.name}"}
              end)
            end
          end
        end

        defmodule FooBarDomain do
          use Ash.Domain

          resources do
            resource FooBar do
              define :hello, args: [:name, :bar]
            end
          end
        end
      end)

    assert String.contains?(output, "Cannot accept the args")
    assert String.contains?(output, ":bar")
  end

  test "a resource defined with a domain can be used with functions in `Ash`" do
    assert %Foo{} =
             Foo
             |> Ash.Changeset.for_create(:create)
             |> Ash.create!()
  end

  describe "fragment-provided resources" do
    defmodule FragmentResource do
      use Ash.Resource,
        domain: Ash.Test.Resource.DomainTest.FragmentDomain,
        data_layer: Ash.DataLayer.Ets

      actions do
        action :first_action, :string do
          run fn _input, _context -> {:ok, "first"} end
        end

        action :second_action, :string do
          run fn _input, _context -> {:ok, "second"} end
        end
      end

      attributes do
        uuid_primary_key :id
      end
    end

    defmodule FragmentDomainFragment do
      use Spark.Dsl.Fragment, of: Ash.Domain

      resources do
        resource Ash.Test.Resource.DomainTest.FragmentResource do
          define :second_action
        end
      end
    end

    defmodule FragmentDomain do
      use Ash.Domain, fragments: [FragmentDomainFragment]

      resources do
        resource FragmentResource do
          define :first_action
        end
      end
    end

    test "resources declared in both the domain and a fragment are deduplicated" do
      assert Ash.Domain.Info.resources(FragmentDomain) == [FragmentResource]
    end

    test "code interfaces from both the domain and the fragment are preserved" do
      definitions =
        FragmentDomain
        |> Ash.Domain.Info.resource_references()
        |> Enum.find(&(&1.resource == FragmentResource))
        |> Map.fetch!(:definitions)

      assert Enum.map(definitions, & &1.name) |> Enum.sort() == [:first_action, :second_action]
    end
  end
end
