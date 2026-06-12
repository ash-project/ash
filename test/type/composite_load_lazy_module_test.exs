# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.CompositeLoadLazyModuleTest do
  @moduledoc false
  # async: false — purges Ash.Type.Keyword to simulate a VM where the module
  # has not been loaded yet (e.g. any `mix test` run where nothing compiles
  # in-VM, so modules load lazily on first call).
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule NestedDoc do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: :embedded

    attributes do
      uuid_primary_key :id, writable?: true
      attribute :note, :string, public?: true
    end

    actions do
      defaults [:read]
    end
  end

  test "keyword values keep their shape when Ash.Type.Keyword is not loaded yet" do
    :code.purge(Ash.Type.Keyword)
    :code.delete(Ash.Type.Keyword)
    refute :erlang.module_loaded(Ash.Type.Keyword)

    constraints = [
      fields: [
        nested: [type: :struct, constraints: [instance_of: NestedDoc]]
      ]
    ]

    values = [[nested: %NestedDoc{id: Ash.UUID.generate(), note: "hello"}]]
    context = %{domain: Domain, actor: nil, tenant: nil, tracer: nil, authorize?: false}

    {:ok, [loaded]} = Ash.Type.load(Ash.Type.Keyword, values, nil, constraints, context)

    assert Keyword.keyword?(loaded)
    assert %NestedDoc{note: "hello"} = loaded[:nested]
  end
end
