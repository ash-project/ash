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

  test "a resource defined with a domain can be used with functions in `Ash`" do
    assert %Foo{} =
             Foo
             |> Ash.Changeset.for_create(:create)
             |> Ash.create!()
  end
end
