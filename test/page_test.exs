# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  describe "keyset cursor input validation (GHSA-j35q-v8h8-7mwq)" do
    alias Ash.Page.Keyset

    setup do
      %{query: Ash.Query.new(ThisTest.Obj), sort: [{:id, :asc}]}
    end

    test "accepts a normal, small cursor", %{query: query, sort: sort} do
      cursor = Base.encode64(:erlang.term_to_binary([Ash.UUID.generate()]))
      assert {:ok, _} = Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects an oversized uncompressed cursor", %{query: query, sort: sort} do
      # A single sort value (matching the one sort field) large enough to exceed
      # the byte-size cap, so this exercises the size guard rather than a shape
      # mismatch.
      cursor = Base.encode64(:erlang.term_to_binary([List.duplicate(0, 20_000)]))

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects the reported keyset bomb (compressed list of integers)",
         %{query: query, sort: sort} do
      # Ash never emits compressed cursors, so any compressed payload is hostile
      # and is rejected without inflating it. The compressed form is tiny on the
      # wire yet would inflate to tens of MB.
      bomb =
        List.duplicate(0, 2_000_000)
        |> :erlang.term_to_binary([{:compressed, 9}])
        |> Base.encode64()

      assert byte_size(bomb) < 10_000
      assert {:error, %Ash.Error.Page.InvalidKeyset{}} = Keyset.filter(query, bomb, sort, :after)
    end

    test "rejects a cursor whose decoded value contains an expression (GHSA-3gq3-9xm3-c8v3)",
         %{query: query, sort: sort} do
      # A forged cursor whose sort value is an `Ash.Query.Call` (an expression),
      # which would otherwise be spliced into the filter as a value and evaluated
      # as a fragment -> SQL injection / RCE depending on the data layer.
      call = %Ash.Query.Call{name: :fragment, args: ["arbitrary"], relationship_path: []}
      cursor = Base.encode64(:erlang.term_to_binary([call]))

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects a cursor whose value can't be cast to the sort field's type",
         %{query: query, sort: sort} do
      # `:id` is a uuid; a value that isn't a valid uuid can never be a legitimate
      # keyset value for that field and is rejected on ingest.
      cursor = Base.encode64(:erlang.term_to_binary([%{not: "a uuid"}]))

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end
  end
end
