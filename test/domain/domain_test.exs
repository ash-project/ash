# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
        assert e.line == 1
        assert String.contains?(e.description, "use Ash.Domain")
        assert String.contains?(e.description, "only one")
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
end
