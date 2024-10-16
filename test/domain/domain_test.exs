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

  test "cannot define a resource that points to a domain that doesn't accept it" do
    assert_raise RuntimeError, ~r/domain does not accept this resource/, fn ->
      defmodule NoResourcesDomain do
        use Ash.Domain
      end

      defmodule Bar do
        use Ash.Resource, domain: NoResourcesDomain

        attributes do
          uuid_primary_key :id
        end
      end
    end
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
    error_pattern =
      ~r/Cannot accept the args `\[:bar\]` because they are not arguments or attributes supported by the `:hello` action/

    assert_raise Spark.Error.DslError, error_pattern, fn ->
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
    end
  end

  test "a resource defined with a domain can be used with functions in `Ash`" do
    assert %Foo{} =
             Foo
             |> Ash.Changeset.for_create(:create)
             |> Ash.create!()
  end
end
