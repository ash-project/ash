defmodule Ash.Test.Resource.PolyResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Aircraft do
    use Ash.Resource

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Ship do
    use Ash.Resource

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Truck do
    use Ash.Resource

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule Crate do
    @moduledoc false
    use Ash.Resource

    attributes do
      uuid_primary_key :id
    end

    relationships do
      poly_belongs_to :transport do
        types %{
          Aircraft => "aircraft",
          Ship => "ship",
          Truck => "truck"
        }

        type_attribute :transport_type
        source_attribute :transport_id
        destination_attribute :id
      end
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Aircraft
      entry Ship
      entry Truck
      entry Crate
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "nothing at all" do
    assert true
  end
end
