# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

alias Ash.Test.PageTest, as: ThisTest

defmodule ThisTest.Obj do
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    defaults [:read, create: :*]

    read :list_objs do
      pagination do
        keyset? true
        offset? true
        default_limit 3
        max_page_size 5
      end
    end
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :name, :string, allow_nil?: false, public?: true
  end
end

defmodule ThisTest.Domain do
  use Ash.Domain

  resources do
    resource ThisTest.Obj do
      define :create_obj, action: :create
      define :list_objs, action: :list_objs
    end
  end
end

defmodule Ash.Test.PageTest do
  @moduledoc false
  use ExUnit.Case, async: true

  setup_all do
    ids =
      for i <- 1..10 do
        obj = ThisTest.Domain.create_obj!(%{name: "obj_#{i}"})
        obj.id
      end

    %{ids: ids}
  end

  describe "keyset" do
    test "pass", %{ids: ids} do
      p1 = ThisTest.Domain.list_objs!()
      assert %Ash.Page.Keyset{results: [_, _, _], more?: true} = p1
      assert p1.results |> Enum.map(& &1.id) == Enum.drop(ids, 0) |> Enum.take(3)

      assert %{results: [_, _, _], more?: true} = p2 = Ash.page!(p1, :next)
      assert p2.results |> Enum.map(& &1.id) == Enum.drop(ids, 3) |> Enum.take(3)

      assert %{results: [_, _, _], more?: true} = p3 = Ash.page!(p2, :next)
      assert p3.results |> Enum.map(& &1.id) == Enum.drop(ids, 6) |> Enum.take(3)

      assert %{results: [_], more?: false} = p4 = Ash.page!(p3, :next)
      assert p4.results |> Enum.map(& &1.id) == Enum.drop(ids, 9) |> Enum.take(3)

      assert %{results: [], more?: false} = p5 = Ash.page!(p4, :next)
      assert p5.results |> Enum.map(& &1.id) == []

      assert %{results: [], more?: false} = p6 = Ash.page!(p5, :next)
      assert p6.results |> Enum.map(& &1.id) == []
    end
  end

  describe "offset" do
    test "pass", %{ids: ids} do
      p1 = ThisTest.Domain.list_objs!(page: [offset: 0])
      assert %Ash.Page.Offset{results: [_, _, _], more?: true} = p1
      assert p1.results |> Enum.map(& &1.id) == Enum.drop(ids, 0) |> Enum.take(3)

      assert %{results: [_, _, _], more?: true} = p2 = Ash.page!(p1, :next)
      assert p2.results |> Enum.map(& &1.id) == Enum.drop(ids, 3) |> Enum.take(3)

      assert %{results: [_, _, _], more?: true} = p3 = Ash.page!(p2, :next)
      assert p3.results |> Enum.map(& &1.id) == Enum.drop(ids, 6) |> Enum.take(3)

      assert %{results: [_], more?: false} = p4 = Ash.page!(p3, :next)
      assert p4.results |> Enum.map(& &1.id) == Enum.drop(ids, 9) |> Enum.take(3)

      assert %{results: [], more?: false} = p5 = Ash.page!(p4, :next)
      assert p5.results |> Enum.map(& &1.id) == []

      assert %{results: [], more?: false} = p6 = Ash.page!(p5, :next)
      assert p6.results |> Enum.map(& &1.id) == []
    end
  end
end
