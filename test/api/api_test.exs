defmodule Ash.Test.Resource.ApiTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Api do
    use Ash.Api

    resources do
      allow_unregistered? true
    end
  end

  defmodule Foo do
    use Ash.Resource, api: Api

    actions do
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id
    end
  end

  test "cannot define a resource that points to an api that doesn't accept it" do
    assert_raise RuntimeError, ~r/api does not accept this resource/, fn ->
      defmodule NoResourcesApi do
        use Ash.Api
      end

      defmodule Bar do
        use Ash.Resource, api: NoResourcesApi

        attributes do
          uuid_primary_key :id
        end
      end
    end
  end

  test "a resource defined with an api can be used with functions in `Ash`" do
    assert %Foo{} =
             Foo
             |> Ash.Changeset.for_create(:create)
             |> Ash.create!()
  end
end
