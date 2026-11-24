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
      prepare after_action(fn query, results, _ ->
                if pid = query.context[:test_pid] do
                  send(pid, {:after_action_count, length(results)})
                end

                {:ok, results}
              end)

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
    attribute :email, :ci_string, public?: true
    attribute :data, :term, public?: true
  end
end

defmodule ThisTest.Scored do
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    defaults [:read, create: :*]
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :score, :integer, allow_nil?: false, public?: true
  end

  relationships do
    has_many :children, ThisTest.ScoredChild, public?: true
  end

  calculations do
    calculate :strict_score, :integer, expr(score), allow_nil?: false, public?: true
    calculate :loose_score, :integer, expr(score), public?: true
  end

  aggregates do
    count :child_count, :children, public?: true
    sum :child_total, :children, :score, default: 0, public?: true
    sum :child_total_nullable, :children, :score, public?: true
  end
end

defmodule ThisTest.ScoredChild do
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets

  actions do
    defaults [:read, create: :*]
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :score, :integer, public?: true
  end

  relationships do
    belongs_to :scored, ThisTest.Scored, public?: true
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
      p1 = ThisTest.Domain.list_objs!(page: [offset: 0], context: %{test_pid: self()})

      # Default limit is 3
      assert_received {:after_action_count, 3}

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

  describe "keyset cursor input validation (GHSA-j35q-v8h8-7mwq, GHSA-m5v6-3qwf-6w9r)" do
    alias Ash.Page.Keyset

    setup do
      %{query: Ash.Query.new(ThisTest.Obj), sort: [{:id, :asc}]}
    end

    test "accepts a normal, small cursor", %{query: query, sort: sort} do
      assert {:ok, _} = Keyset.filter(query, json_keyset([Ash.UUID.generate()]), sort, :after)
    end

    test "rejects an oversized cursor", %{query: query, sort: sort} do
      cursor = json_keyset([String.duplicate("a", 20_000)])

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects a JSON cursor that isn't a list", %{query: query, sort: sort} do
      cursor = json_keyset(%{"id" => Ash.UUID.generate()})

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects a term cursor without a signing secret", %{query: query, sort: sort} do
      cursor = Base.encode64(:erlang.term_to_binary([Ash.UUID.generate()]))

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects a term cursor containing a gadget struct", %{query: query} do
      # `[:safe]` deserialization produces structs built from atoms that already
      # exist, so a term cursor must never be decoded unless it is signed. Here an
      # Ecto dynamic wrapped in an `Ash.CiString`, which `:ci_string` casting
      # passes through.
      sort = [{:email, :asc}, {:id, :asc}]
      gadget = struct(Ecto.Query.DynamicExpr, binding: :gadget)
      term = [struct(Ash.CiString, string: {gadget}), Ash.UUID.generate()]
      cursor = Base.encode64(:erlang.term_to_binary(term))

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "a JSON cursor can't express a struct", %{query: query} do
      # The closest JSON gets to a struct is a map, which `:ci_string` won't cast
      sort = [{:email, :asc}, {:id, :asc}]
      cursor = json_keyset([%{"__struct__" => "Elixir.Ash.CiString"}, Ash.UUID.generate()])

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end

    test "rejects a cursor whose value can't be cast to the sort field's type",
         %{query: query, sort: sort} do
      # `:id` is a uuid; a value that isn't a valid uuid can never be a legitimate
      # keyset value for that field and is rejected on ingest.
      cursor = json_keyset([%{"not" => "a uuid"}])

      assert {:error, %Ash.Error.Page.InvalidKeyset{}} =
               Keyset.filter(query, cursor, sort, :after)
    end
  end

  describe "keyset encoding" do
    alias Ash.Page.Keyset

    defp obj(attrs) do
      struct(
        ThisTest.Obj,
        Map.merge(%{id: Ash.UUID.generate(), name: "a", __metadata__: %{}}, attrs)
      )
    end

    defp keyset_of(record, sort) do
      [%{__metadata__: %{keyset: keyset}}] =
        Keyset.data_with_keyset([record], ThisTest.Obj, sort)

      keyset
    end

    test "values that survive a JSON round trip are encoded as JSON" do
      record = obj(%{email: Ash.CiString.new("Some@Example.com")})
      sort = [{:email, :asc}, {:id, :asc}]
      keyset = keyset_of(record, sort)

      assert keyset == json_keyset(["Some@Example.com", record.id])
      assert {:ok, _} = Keyset.filter(Ash.Query.new(ThisTest.Obj), keyset, sort, :after)
    end

    test "values with no JSON form have no keyset without a signing secret" do
      record = obj(%{data: {:a, 1}})
      sort = [{:data, :asc}, {:id, :asc}]

      assert keyset_of(record, sort) == nil

      assert_raise ArgumentError, ~r/sorted by :data.*keyset_signing_secret/s, fn ->
        Keyset.ensure_keysets!([%{record | __metadata__: %{keyset: nil}}], ThisTest.Obj, sort)
      end
    end
  end

  describe "keyset filters and allow_nil?" do
    alias Ash.Page.Keyset
    require Ash.Expr

    defp keyset_filter(query) do
      cursor = json_keyset([5])
      {:ok, filter} = Keyset.filter(query, cursor, query.sort, :after)
      filter
    end

    defp checks_nil?(filter), do: inspect(filter) =~ "is_nil"

    defp anonymous_score(opts) do
      Ash.Query.Calculation.new(
        :anonymous_score,
        Ash.Resource.Calculation.Expression,
        [expr: Ash.Expr.expr(score)],
        :integer,
        [],
        opts
      )
    end

    test "an allow_nil?: false attribute sort has no nil checks" do
      refute ThisTest.Scored |> Ash.Query.sort(score: :asc) |> keyset_filter() |> checks_nil?()
    end

    test "an allow_nil?: false calculation sort has no nil checks" do
      refute ThisTest.Scored
             |> Ash.Query.sort(strict_score: :asc)
             |> keyset_filter()
             |> checks_nil?()
    end

    test "a nullable calculation sort keeps its nil checks" do
      assert ThisTest.Scored
             |> Ash.Query.sort(loose_score: :asc)
             |> keyset_filter()
             |> checks_nil?()
    end

    test "an anonymous calculation can declare allow_nil?: false" do
      {:ok, calc} = anonymous_score(allow_nil?: false)

      refute ThisTest.Scored
             |> Ash.Query.sort([{calc, :asc}])
             |> keyset_filter()
             |> checks_nil?()
    end

    test "an aggregate that is never nil has no nil checks" do
      refute ThisTest.Scored
             |> Ash.Query.sort(child_count: :asc)
             |> keyset_filter()
             |> checks_nil?()
    end

    test "an aggregate with a default has no nil checks" do
      refute ThisTest.Scored
             |> Ash.Query.sort(child_total: :asc)
             |> keyset_filter()
             |> checks_nil?()
    end

    test "a nullable aggregate keeps its nil checks" do
      assert ThisTest.Scored
             |> Ash.Query.sort(child_total_nullable: :asc)
             |> keyset_filter()
             |> checks_nil?()
    end

    test "an anonymous calculation is nullable by default" do
      {:ok, calc} = anonymous_score([])

      assert ThisTest.Scored
             |> Ash.Query.sort([{calc, :asc}])
             |> keyset_filter()
             |> checks_nil?()
    end
  end

  describe "after_action hooks do not see the extra pagination row" do
    setup do
      %{opts: [context: %{test_pid: self()}]}
    end

    test "offset pagination", %{ids: ids, opts: opts} do
      page = ThisTest.Domain.list_objs!([page: [offset: 0, limit: 3]] ++ opts)

      assert_received {:after_action_count, 3}
      assert %Ash.Page.Offset{results: [_, _, _], more?: true} = page
      assert Enum.map(page.results, & &1.id) == Enum.take(ids, 3)
    end

    test "keyset pagination", %{ids: ids, opts: opts} do
      page = ThisTest.Domain.list_objs!([page: [limit: 3]] ++ opts)
      assert_received {:after_action_count, 3}
      assert %Ash.Page.Keyset{more?: true} = page

      next = Ash.page!(page, :next)
      assert_received {:after_action_count, 3}
      assert %{more?: true} = next
      assert Enum.map(next.results, & &1.id) == ids |> Enum.drop(3) |> Enum.take(3)
    end

    test "keyset pagination going backwards", %{ids: ids, opts: opts} do
      # Page forward twice so that the `before:` cursor has more records behind
      # it than the limit -- otherwise there is no extra row to leak.
      third_page =
        ThisTest.Domain.list_objs!([page: [limit: 3]] ++ opts)
        |> Ash.page!(:next)
        |> Ash.page!(:next)

      assert Enum.map(third_page.results, & &1.id) == ids |> Enum.drop(6) |> Enum.take(3)
      # One message per page fetched so far.
      assert_received {:after_action_count, 3}
      assert_received {:after_action_count, 3}
      assert_received {:after_action_count, 3}

      previous = Ash.page!(third_page, :prev)

      assert_received {:after_action_count, 3}
      assert %{more?: true} = previous
      assert Enum.map(previous.results, & &1.id) == ids |> Enum.drop(3) |> Enum.take(3)
    end

    test "the last page sees only the records that exist", %{ids: ids, opts: opts} do
      last = ThisTest.Domain.list_objs!([page: [offset: 9, limit: 3]] ++ opts)

      assert_received {:after_action_count, 1}
      assert %Ash.Page.Offset{results: [_], more?: false} = last
      assert Enum.map(last.results, & &1.id) == Enum.drop(ids, 9)
    end

    test "an exactly-full last page reports more?: false", %{opts: opts} do
      page = ThisTest.Domain.list_objs!([page: [offset: 5, limit: 5]] ++ opts)

      assert_received {:after_action_count, 5}
      assert %Ash.Page.Offset{more?: false} = page
    end

    test "read_one skips pagination entirely, so the hook sees every record" do
      assert {:error, %Ash.Error.Invalid{}} =
               ThisTest.Obj
               |> Ash.Query.for_read(:list_objs)
               |> Ash.Query.set_context(%{test_pid: self()})
               |> Ash.read_one()

      assert_received {:after_action_count, 10}
    end
  end

  describe "data_layer_query/2" do
    defp data_layer_query(page_opts) do
      ThisTest.Obj
      |> Ash.Query.for_read(:list_objs)
      |> Ash.Query.set_context(%{test_pid: self()})
      |> Ash.Query.page(page_opts)
      |> Ash.data_layer_query!()
    end

    test "run returns an offset page without the extra row", %{ids: ids} do
      %{run: run, query: query} = data_layer_query(offset: 0, limit: 3)

      assert {:ok, %Ash.Page.Offset{results: [_, _, _], more?: true, count: nil} = page} =
               run.(query)

      assert Enum.map(page.results, & &1.id) == Enum.take(ids, 3)
      assert_received {:after_action_count, 3}
    end

    test "run resolves the count when it is requested" do
      %{run: run, query: query} = data_layer_query(offset: 0, limit: 3, count: true)

      assert {:ok, %Ash.Page.Offset{count: 10}} = run.(query)
    end

    test "run returns a keyset page that can be paged from", %{ids: ids} do
      %{run: run, query: query} = data_layer_query(limit: 3)

      assert {:ok, %Ash.Page.Keyset{results: [_, _, _], more?: true} = page} = run.(query)
      assert Enum.all?(page.results, &(&1.__metadata__.keyset != nil))
      assert_received {:after_action_count, 3}

      next = Ash.page!(page, :next)
      assert Enum.map(next.results, & &1.id) == ids |> Enum.drop(3) |> Enum.take(3)
    end

    test "run reports more?: false on the last pages", %{ids: ids} do
      %{run: run, query: query} = data_layer_query(offset: 5, limit: 5)
      assert {:ok, %Ash.Page.Offset{results: [_, _, _, _, _], more?: false}} = run.(query)
      assert_received {:after_action_count, 5}

      %{run: run, query: query} = data_layer_query(offset: 9, limit: 3)
      assert {:ok, %Ash.Page.Offset{results: [last], more?: false}} = run.(query)
      assert last.id == List.last(ids)
      assert_received {:after_action_count, 1}
    end

    test "load accepts the page run returned and gives back a page" do
      %{run: run, load: load, query: query, ash_query: ash_query} =
        data_layer_query(offset: 0, limit: 3, count: true)

      assert {:ok, %Ash.Page.Offset{} = page} = run.(query)
      assert {:ok, %Ash.Page.Offset{} = loaded} = load.(ash_query, page)

      assert loaded.results == page.results
      assert loaded.more? == page.more?
      assert loaded.count == page.count
    end

    test "load builds the page from raw rows that still contain the extra row", %{ids: ids} do
      %{load: load, query: query, ash_query: ash_query} = data_layer_query(offset: 0, limit: 3)

      {:ok, raw_rows} = Ash.DataLayer.run_query(query, ThisTest.Obj)
      assert length(raw_rows) == 4

      assert {:ok, %Ash.Page.Offset{results: [_, _, _], more?: true} = page} =
               load.(ash_query, raw_rows)

      assert Enum.map(page.results, & &1.id) == Enum.take(ids, 3)
    end

    test "run caps the page at max_page_size and reports more?", %{ids: ids} do
      %{run: run, query: query} = data_layer_query(offset: 0, limit: 100)

      assert {:ok, %Ash.Page.Offset{results: results, limit: 5, more?: true}} = run.(query)
      assert Enum.map(results, & &1.id) == Enum.take(ids, 5)
      assert_received {:after_action_count, 5}
    end

    test "load caps raw rows at max_page_size and reports more?", %{ids: ids} do
      %{load: load, query: query, ash_query: ash_query} = data_layer_query(offset: 0, limit: 100)

      {:ok, raw_rows} = Ash.DataLayer.run_query(query, ThisTest.Obj)
      assert length(raw_rows) == 6

      assert {:ok, %Ash.Page.Offset{results: results, limit: 5, more?: true}} =
               load.(ash_query, raw_rows)

      assert Enum.map(results, & &1.id) == Enum.take(ids, 5)
    end
  end

  defp json_keyset(values) do
    Base.encode64(Jason.encode!(values))
  end
end
