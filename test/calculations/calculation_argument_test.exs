# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.CalculationArgumentExprTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr

  @ids ["7f6f4c9c-49a8-4c5e-9d43-3a8e7d6bb9f0", "0b0d0a5e-22a8-4f3e-9a9c-7d1f0f6c4e11"]

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Label do
    @moduledoc false
    use Ash.Type

    def storage_type(_), do: :map
    def cast_input(value, _) when is_binary(value), do: {:ok, %{value: value}}
    def cast_input(_, _), do: :error
    def cast_stored(value, _), do: {:ok, value}
    def dump_to_native(value, _), do: {:ok, value}

    def apply_constraints(value, _) do
      {:ok, %{value | value: String.trim(value.value)}}
    end
  end

  defmodule Post do
    @moduledoc false
    @ids ["7f6f4c9c-49a8-4c5e-9d43-3a8e7d6bb9f0", "0b0d0a5e-22a8-4f3e-9a9c-7d1f0f6c4e11"]

    use Ash.Resource,
      domain: Ash.Test.Calculations.CalculationArgumentExprTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*]
    end

    attributes do
      uuid_primary_key :id, writable?: true, public?: true
      attribute :title, :string, public?: true
      attribute :published_at, :utc_datetime_usec, public?: true
      attribute :cutoff, :utc_datetime_usec, public?: true
      attribute :tags, {:array, :string}, public?: true
    end

    calculations do
      calculate :cutoff_calc, :utc_datetime_usec, expr(cutoff), public?: true

      calculate :tags_are?, :boolean, expr(tags == ^arg(:tags)) do
        public? true
        argument :tags, {:array, :string}, allow_nil?: true, allow_expr?: true
      end

      # array argument without allow_expr?
      calculate :in_ids_after?,
                :boolean,
                expr(id in ^arg(:ids) and published_at > ^arg(:after)) do
        public? true
        argument :ids, {:array, :uuid}, allow_nil?: false
        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      # array argument with allow_expr?
      calculate :in_expr_ids_after?,
                :boolean,
                expr(id in ^arg(:ids) and published_at > ^arg(:after)) do
        public? true
        argument :ids, {:array, :uuid}, allow_nil?: false, allow_expr?: true
        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      # scalar argument without allow_expr?, for comparison
      calculate :titled_after?,
                :boolean,
                expr(title == ^arg(:title) and published_at > ^arg(:after)) do
        public? true
        argument :title, :string, allow_nil?: false
        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      calculate :optional_title_after?,
                :boolean,
                expr(is_nil(^arg(:title)) and published_at > ^arg(:after)) do
        argument :title, :string
        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      calculate :default_title_after?,
                :boolean,
                expr(title == ^arg(:title) and published_at > ^arg(:after)) do
        argument :title, :string, allow_nil?: false, default: fn -> "match" end
        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      calculate :label_after?,
                :boolean,
                expr(get_path(^arg(:label), [:value]) == title and published_at > ^arg(:after)) do
        argument :label, Ash.Test.Calculations.CalculationArgumentExprTest.Label,
          allow_nil?: false

        argument :after, :utc_datetime_usec, allow_nil?: false, allow_expr?: true
      end

      calculate :matches_in_ids_after?,
                :boolean,
                expr(
                  in_ids_after?(
                    ids: ^@ids,
                    after: cutoff_calc
                  )
                ),
                public?: true

      calculate :matches_in_expr_ids_after?,
                :boolean,
                expr(
                  in_expr_ids_after?(
                    ids: ^@ids,
                    after: cutoff_calc
                  )
                ),
                public?: true

      calculate :matches_titled_after?,
                :boolean,
                expr(titled_after?(title: "match", after: cutoff_calc)),
                public?: true
    end
  end

  @cutoff ~U[2026-01-01 00:00:00.000000Z]

  setup do
    [matching_id, other_id] = @ids

    for {id, title, published_at} <- [
          {matching_id, "match", ~U[2026-02-01 00:00:00.000000Z]},
          {matching_id |> String.replace_prefix("7", "8"), "match",
           ~U[2026-02-01 00:00:00.000000Z]},
          {other_id, "other", ~U[2025-12-01 00:00:00.000000Z]}
        ] do
      Post
      |> Ash.Changeset.for_create(:create, %{
        id: id,
        title: title,
        published_at: published_at,
        cutoff: @cutoff
      })
      |> Ash.create!()
    end

    %{matching_id: matching_id}
  end

  describe "a literal array argument alongside an expression argument" do
    test "loads through another expression calculation", %{matching_id: matching_id} do
      # Ash.Query.load(:matches_in_ids_after?)
      #   -> in_ids_after?(ids: ^@ids, after: cutoff_calc)
      #        -> cutoff_calc -> cutoff (the current post's field)
      #        -> id in ^arg(:ids) and published_at > ^arg(:after)
      # The literal UUID array must survive validation beside the calculated timestamp.
      # Only the post both in @ids and published after its cutoff should load true.

      results =
        Post
        |> Ash.Query.load(:matches_in_ids_after?)
        |> Ash.read!()
        |> Map.new(&{&1.id, &1.matches_in_ids_after?})

      assert results[matching_id] == true
      assert results |> Map.delete(matching_id) |> Map.values() |> Enum.all?(&(&1 == false))
    end

    test "filters through another expression calculation", %{matching_id: matching_id} do
      # filter(matches_in_ids_after?)
      #   -> in_ids_after?(ids: ^@ids, after: cutoff_calc)
      #        -> cutoff_calc -> cutoff
      #        -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Filtering through the outer calculation must preserve the literal array and
      # resolve the timestamp per post, returning only the matching ID.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.filter(matches_in_ids_after?)
               |> Ash.read!()
    end

    test "works when the array argument allows expressions", %{matching_id: matching_id} do
      # filter(matches_in_expr_ids_after?)
      #   -> in_expr_ids_after?(ids: ^@ids, after: cutoff_calc)
      #        -> cutoff_calc -> cutoff
      #        -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Even though :ids allows expressions, this supplied array is still a literal.
      # It must work beside the calculated timestamp and select only the matching ID.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.filter(matches_in_expr_ids_after?)
               |> Ash.read!()
    end

    test "works in a query filter", %{matching_id: matching_id} do
      # filter(in_ids_after?(ids: ^@ids, after: cutoff_calc))
      #   -> cutoff_calc -> cutoff
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # The direct expression call must preserve the literal array beside the
      # calculated timestamp without needing an outer calculation.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.filter(in_ids_after?(ids: ^@ids, after: cutoff_calc))
               |> Ash.read!()
    end

    test "works in a filter statement", %{matching_id: matching_id} do
      # do_filter(map input) -> in_ids_after?(ids: @ids, after: expr(cutoff_calc))
      #   -> cutoff_calc -> cutoff
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Map-form parsing must preserve the literal array beside an expression argument
      # and apply the calculation's eq: true predicate to select the matching ID.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.do_filter(%{
                 in_ids_after?: %{input: %{ids: @ids, after: expr(cutoff_calc)}, eq: true}
               })
               |> Ash.read!()
    end
  end

  describe "the tuple filter form" do
    test "accepts a literal array argument alongside an expression argument", %{
      matching_id: matching_id
    } do
      # do_filter({arguments, predicates})
      #   -> in_ids_after?(ids: @ids, after: expr(cutoff_calc))
      #        -> cutoff_calc -> cutoff
      #        -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Tuple-form parsing must accept the same literal/expression combination and
      # select only the matching ID with eq: true.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.do_filter(
                 in_ids_after?: {%{ids: @ids, after: expr(cutoff_calc)}, [eq: true]}
               )
               |> Ash.read!()
    end

    test "accepts string-keyed arguments", %{matching_id: matching_id} do
      # do_filter(tuple input with string keys)
      #   -> in_ids_after?(ids: @ids, after: @cutoff)
      #        -> id in ^arg(:ids) and published_at > ^arg(:after)
      # String argument names must resolve to the declared arguments through the
      # same tuple path; both values here are literals.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.do_filter(
                 in_ids_after?: {%{"ids" => @ids, "after" => @cutoff}, [eq: true]}
               )
               |> Ash.read!()
    end
  end

  describe "filter input" do
    test "accepts literal calculation arguments", %{matching_id: matching_id} do
      # filter_input(map input) -> in_ids_after?(ids: @ids, after: @cutoff)
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # The public input API must accept these literal arguments and return only
      # the post satisfying both conditions.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.filter_input(%{
                 in_ids_after?: %{input: %{ids: @ids, after: @cutoff}, eq: true}
               })
               |> Ash.read!()
    end

    test "does not accept expressions as calculation arguments" do
      # filter_input(map input) -> in_ids_after?(ids: @ids, after: expr(cutoff_calc))
      #   -> argument validation rejects the expression before calculation evaluation.
      # Even though :after allows expressions in developer expressions, filter_input
      # must reject this expression with the useful "does not support expressions" error.

      assert_raise Ash.Error.Invalid, ~r/does not support expressions/, fn ->
        Post
        |> Ash.Query.filter_input(%{
          in_ids_after?: %{input: %{ids: @ids, after: expr(cutoff_calc)}, eq: true}
        })
        |> Ash.read!()
      end
    end
  end

  describe "string-keyed calculation arguments" do
    test "are accepted in filter input", %{matching_id: matching_id} do
      # filter_input(string keys) -> in_ids_after?(ids: @ids, after: timestamp string)
      #   -> cast :ids as UUID array and :after as utc_datetime_usec
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Both string argument names must resolve, the timestamp must cast, and only
      # the matching post should pass eq: true.

      assert [%{id: ^matching_id}] =
               Post
               |> Ash.Query.filter_input(%{
                 "in_ids_after?" => %{
                   "input" => %{"ids" => @ids, "after" => "2026-01-01T00:00:00Z"},
                   "eq" => true
                 }
               })
               |> Ash.read!()
    end

    test "are accepted in sort input", %{matching_id: matching_id} do
      # sort_input(string keys) -> in_ids_after?(ids: @ids, after: timestamp string)
      #   -> cast the named arguments
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Descending boolean order must put the true result first after accepting
      # string argument names and casting the timestamp.

      assert [%{id: ^matching_id} | _] =
               Post
               |> Ash.Query.sort_input([
                 {"in_ids_after?",
                  {%{"ids" => @ids, "after" => "2026-01-01T00:00:00Z"}, :desc_nils_last}}
               ])
               |> Ash.read!()
    end

    test "are accepted when loading the calculation", %{matching_id: matching_id} do
      # load(string keys) -> in_ids_after?(ids: @ids, after: timestamp string)
      #   -> cast the named arguments
      #   -> id in ^arg(:ids) and published_at > ^arg(:after)
      # Loading must accept string argument names and expose true on the matching post.

      results =
        Post
        |> Ash.Query.load(in_ids_after?: %{"ids" => @ids, "after" => "2026-01-01T00:00:00Z"})
        |> Ash.read!()
        |> Map.new(&{&1.id, &1.in_ids_after?})

      assert results[matching_id] == true
    end
  end

  describe "an expression for an array argument" do
    setup do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{tags: ["a", "b"]})
        |> Ash.create!()

      %{post: post}
    end

    test "works in a filter", %{post: post} do
      # filter(tags_are?(tags: tags)) -> tags == ^arg(:tags)
      #   -> resolve the argument from the current post's tags field.
      # An array-valued expression must bypass literal-array preprocessing and match
      # the post whose tags are ["a", "b"].

      assert [%{id: id}] = Post |> Ash.Query.filter(tags_are?(tags: tags)) |> Ash.read!()
      assert id == post.id
    end

    test "works when loading the calculation", %{post: post} do
      # load(tags_are?: %{tags: expr(tags)}) -> tags == ^arg(:tags)
      #   -> resolve the argument from the selected post's tags field.
      # Loading must accept an array-valued expression and return true, rather than
      # trying to preprocess the expression itself as a literal array.

      assert %{tags_are?: true} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.Query.load(tags_are?: %{tags: expr(tags)})
               |> Ash.read_one!()
    end
  end

  describe "invalid calculation input" do
    for input <- [nil, "x", [1, 2]] do
      test "in filter input #{inspect(input)} is an invalid filter" do
        # filter_input(in_ids_after? with malformed input)
        #   -> reject the argument container before evaluating the calculation.
        # Each parameterized shape (nil, a string, or a non-keyword list) must yield
        # an Ash.Error.Invalid result instead of crashing during argument parsing.

        assert {:error, %Ash.Error.Invalid{}} =
                 Post
                 |> Ash.Query.filter_input(%{
                   "in_ids_after?" => %{"input" => unquote(input), "eq" => true}
                 })
                 |> Ash.read()
      end
    end

    for args <- ["x", [1, 2]] do
      test "in the tuple filter form #{inspect(args)} is an invalid filter" do
        # filter_input(in_ids_after?: {malformed args, eq: true})
        #   -> reject the tuple's argument container before calculation evaluation.
        # Both the string and non-keyword list variants must return Ash.Error.Invalid.

        assert {:error, %Ash.Error.Invalid{}} =
                 Post
                 |> Ash.Query.filter_input(in_ids_after?: {unquote(args), [eq: true]})
                 |> Ash.read()
      end
    end

    for input <- ["x", [1, 2], 5] do
      test "in sort input #{inspect(input)} is an invalid sort" do
        # sort_input(in_ids_after? with malformed input)
        #   -> reject the argument container before evaluating the sort calculation.
        # The string, non-keyword list, and integer variants must return Ash.Error.Invalid.

        assert {:error, %Ash.Error.Invalid{}} =
                 Post
                 |> Ash.Query.sort_input([{"in_ids_after?", {unquote(input), :asc}}])
                 |> Ash.read()
      end
    end

    test "with keys that aren't argument names is an invalid filter" do
      # filter_input(in_ids_after?: %{input: %{1 => 2}, eq: true})
      #   -> argument-name lookup rejects key 1 before calculation evaluation.
      # Unknown keys must reach typed NoSuchInput validation instead of failing
      # in an earlier options-schema check.

      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.NoSuchInput{}]}} =
               Post
               |> Ash.Query.filter_input(%{in_ids_after?: %{input: %{1 => 2}, eq: true}})
               |> Ash.read()
    end

    test "in the tuple filter form does not accept expressions" do
      # filter_input(in_ids_after?: {args, eq: true})
      #   -> rejects after: expr(cutoff_calc) during argument validation.
      # Tuple-form input must enforce the same expression restriction as map-form
      # input and preserve the "does not support expressions" message.

      assert {:error, %Ash.Error.Invalid{} = error} =
               Post
               |> Ash.Query.filter_input(
                 in_ids_after?: {%{ids: @ids, after: expr(cutoff_calc)}, [eq: true]}
               )
               |> Ash.read()

      assert Exception.message(error) =~ "does not support expressions"
    end
  end

  describe "normalized literal calculation arguments" do
    test "filter alongside an expression argument", %{matching_id: matching_id} do
      # filter(titled_after?(title: " match ", after: cutoff_calc))
      #   -> normalize :title to "match"; cutoff_calc -> cutoff
      #   -> title == ^arg(:title) and published_at > ^arg(:after)
      # The normalized literal must survive beside the expression, so both recent
      # posts titled "match" pass (including the one outside @ids).

      records =
        Post
        |> Ash.Query.filter(titled_after?(title: " match ", after: cutoff_calc))
        |> Ash.read!()

      assert matching_id in Enum.map(records, & &1.id)
      assert length(records) == 2
    end

    test "load alongside an expression argument", %{matching_id: matching_id} do
      # load(titled_after?: %{title: " match ", after: expr(cutoff_calc)})
      #   -> normalize :title to "match"; cutoff_calc -> cutoff
      #   -> title == ^arg(:title) and published_at > ^arg(:after)
      # Loading the selected post must use the trimmed literal beside the expression
      # and expose true.

      assert %{titled_after?: true} =
               Post
               |> Ash.Query.filter(id == ^matching_id)
               |> Ash.Query.load(titled_after?: %{title: " match ", after: expr(cutoff_calc)})
               |> Ash.read_one!()
    end

    test "normalize with literal siblings", %{matching_id: matching_id} do
      # load/filter -> titled_after?(title: " match ", after: @cutoff)
      #   -> normalize :title to "match"
      #   -> title == ^arg(:title) and published_at > ^arg(:after)
      # Normalization must also work when both arguments are literals: loading is
      # true on the selected post and filtering returns both recent matching titles.

      assert %{titled_after?: true} =
               Post
               |> Ash.Query.filter(id == ^matching_id)
               |> Ash.Query.load(titled_after?: %{title: " match ", after: @cutoff})
               |> Ash.read_one!()

      assert length(
               Post
               |> Ash.Query.filter(titled_after?(title: " match ", after: ^@cutoff))
               |> Ash.read!()
             ) == 2
    end

    for after_value <- [@cutoff, expr(cutoff_calc)] do
      test "reject a required string normalized to nil with #{inspect(after_value)}" do
        # load(titled_after?) -> normalize the supplied blank :title to nil
        #   -> reject it because :title has allow_nil?: false.
        # Both literal and cutoff_calc expression siblings must produce "is required";
        # an expression sibling must not hide the normalized nil inside a type wrapper.

        query =
          Ash.Query.load(Post,
            titled_after?: %{title: "  ", after: unquote(Macro.escape(after_value))}
          )

        assert {:error, error} = Ash.read(query)
        assert Exception.message(error) =~ "is required"
      end

      test "reject a provided string normalized to nil instead of using its default with #{inspect(after_value)}" do
        # load(default_title_after?) -> normalize the supplied blank :title to nil
        #   -> reject it because :title has allow_nil?: false.
        # The default fn -> "match" end applies only when :title is omitted, so neither
        # literal nor expression siblings may turn this supplied invalid value into a default.

        query =
          Ash.Query.load(Post,
            default_title_after?: %{title: "  ", after: unquote(Macro.escape(after_value))}
          )

        assert {:error, error} = Ash.read(query)
        assert Exception.message(error) =~ "is required"
      end

      test "allow an optional string normalized to nil with #{inspect(after_value)}", %{
        matching_id: matching_id
      } do
        # load(optional_title_after?) -> normalize the supplied blank :title to nil
        #   -> is_nil(^arg(:title)) and published_at > ^arg(:after)
        # The optional argument must retain nil with either a literal timestamp or
        # cutoff_calc expression sibling, making the selected recent post load true.

        assert %{optional_title_after?: true} =
                 Post
                 |> Ash.Query.filter(id == ^matching_id)
                 |> Ash.Query.load(
                   optional_title_after?: %{
                     title: "  ",
                     after: unquote(Macro.escape(after_value))
                   }
                 )
                 |> Ash.read_one!()
      end
    end

    test "preserve default callbacks with expression siblings", %{matching_id: matching_id} do
      # load(default_title_after?: %{after: expr(cutoff_calc)})
      #   -> omitted :title -> default callback -> "match"
      #   -> cutoff_calc -> cutoff
      #   -> title == ^arg(:title) and published_at > ^arg(:after)
      # The expression sibling must preserve the omitted default callback's value.

      assert %{default_title_after?: true} =
               Post
               |> Ash.Query.filter(id == ^matching_id)
               |> Ash.Query.load(default_title_after?: %{after: expr(cutoff_calc)})
               |> Ash.read_one!()
    end

    test "preserve constrained custom values without recasting them", %{matching_id: matching_id} do
      # load/filter -> label_after?(label: " match ", after: cutoff_calc)
      #   -> Label.cast_input -> %{value: " match "}
      #   -> Label.apply_constraints -> %{value: "match"}
      #   -> cutoff_calc -> cutoff
      #   -> get_path(^arg(:label), [:value]) == title and published_at > ^arg(:after)
      # The constrained map must survive without a second cast (Label only accepts
      # strings), making loading true and filtering return both recent matching titles.

      assert %{label_after?: true} =
               Post
               |> Ash.Query.filter(id == ^matching_id)
               |> Ash.Query.load(label_after?: %{label: " match ", after: expr(cutoff_calc)})
               |> Ash.read_one!()

      assert length(
               Post
               |> Ash.Query.filter(label_after?(label: " match ", after: cutoff_calc))
               |> Ash.read!()
             ) == 2
    end

    test "keep expression string coercion distinct from literal normalization" do
      # Ash.Expr.eval(type(" match ", :string)) -> ordinary expression coercion.
      # This explicit type() call must keep its spaces; calculation-argument trimming
      # must not change the behavior of general expression type coercion.

      assert {:ok, " match "} = Ash.Expr.eval(expr(type(" match ", :string)))
    end
  end

  describe "a literal scalar argument alongside an expression argument" do
    test "filters through another expression calculation", %{matching_id: matching_id} do
      # filter(matches_titled_after?)
      #   -> titled_after?(title: "match", after: cutoff_calc)
      #        -> cutoff_calc -> cutoff
      #        -> title == ^arg(:title) and published_at > ^arg(:after)
      # A literal scalar must work beside the calculated timestamp through an outer
      # calculation, returning both recent posts titled "match".

      ids = Post |> Ash.Query.filter(matches_titled_after?) |> Ash.read!() |> Enum.map(& &1.id)
      assert matching_id in ids
      assert length(ids) == 2
    end
  end
end

defmodule Ash.Test.Calculations.CalculationArgumentErrorsTest do
  use ExUnit.Case, async: true
  require Ash.Query
  import Ash.Expr

  defmodule Domain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Label do
    use Ash.Type

    def storage_type(_constraints), do: :string
    def cast_input(value, _constraints) when is_binary(value), do: {:ok, value}
    def cast_input(_value, _constraints), do: :error
    def cast_stored(value, _constraints), do: {:ok, value}
    def dump_to_native(value, _constraints), do: {:ok, value}

    def apply_constraints("constraint-invalid", _constraints), do: :error
    def apply_constraints(value, _constraints), do: {:ok, String.trim(value)}

    def cast_input_array(values, _constraints) do
      if "cast-invalid" in values, do: :error, else: {:ok, values}
    end

    def apply_constraints_array(values, _constraints) do
      cond do
        "constraint-invalid" in values -> :error
        "message-invalid" in values -> {:error, "labels were rejected"}
        true -> {:ok, Enum.map(values, &String.trim/1)}
      end
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    calculations do
      calculate :scalar_matches?, :boolean, expr(title == ^arg(:values)) do
        public? true
        argument :values, Label, allow_nil?: false
      end

      calculate :array_matches?, :boolean, expr(title in ^arg(:values)) do
        public? true
        argument :values, {:array, Label}, allow_nil?: false
      end

      calculate :nested_matches?,
                :boolean,
                expr(^arg(:values) == [["match"]] and title == "match") do
        public? true
        argument :values, {:array, {:array, Label}}, allow_nil?: false
      end
    end
  end

  setup do
    matching = Ash.create!(Post, %{title: "match"})
    other = Ash.create!(Post, %{title: "other"})
    %{matching: matching, other: other}
  end

  defp query(:load, calculation, values) do
    Ash.Query.load(Post, [{calculation, %{values: values}}])
  end

  defp query(:filter, calculation, values) do
    Ash.Query.filter_input(Post, %{
      to_string(calculation) => %{"input" => %{"values" => values}, "eq" => true}
    })
  end

  defp query(:sort, calculation, values) do
    Ash.Query.sort_input(Post, [{to_string(calculation), {%{"values" => values}, :desc}}])
  end

  for {description, calculation, values} <- [
        {"scalar constraints", :scalar_matches?, "constraint-invalid"},
        {"collection casting", :array_matches?, ["cast-invalid"]},
        {"collection constraints", :array_matches?, ["constraint-invalid"]},
        {"nested collection casting", :nested_matches?, [["cast-invalid"]]}
      ],
      operation <- [:load, :filter, :sort] do
    test "#{operation} returns input errors for rejected #{description}" do
      # query(operation, calculation, values) -> load/filter_input/sort_input
      #   -> validate :values using Label's scalar or collection callbacks.
      # The scalar constraints, collection casting/constraints, and nested collection
      # casting variants deliberately return :error. Every operation must surface
      # Ash.Error.Invalid instead of crashing before the calculation can run.

      assert {:error, %Ash.Error.Invalid{}} =
               unquote(operation)
               |> query(unquote(calculation), unquote(Macro.escape(values)))
               |> Ash.read()
    end
  end

  for {calculation, values} <- [
        {:scalar_matches?, " match "},
        {:array_matches?, [" match "]},
        {:nested_matches?, [[" match "]]}
      ] do
    test "#{calculation} uses normalized arguments for loads, filters and sorts", context do
      # query(:load/:filter/:sort, calculation, values) -> normalize :values with Label
      #   -> scalar_matches?: title == ^arg(:values)
      #   -> array_matches?: title in ^arg(:values)
      #   -> nested_matches?: ^arg(:values) == [["match"]] and title == "match"
      # Trimming must survive in scalar, array, and nested-array arguments: loading
      # is true only on the matching post, filtering keeps it, and descending sorting
      # puts it before the nonmatching post.

      calculation = unquote(calculation)
      values = unquote(Macro.escape(values))

      loaded = query(:load, calculation, values) |> Ash.read!() |> Map.new(&{&1.id, &1})
      assert Map.fetch!(loaded[context.matching.id], calculation)
      refute Map.fetch!(loaded[context.other.id], calculation)

      assert [found] = query(:filter, calculation, values) |> Ash.read!()
      assert found.id == context.matching.id

      assert [first, second] = query(:sort, calculation, values) |> Ash.read!()
      assert first.id == context.matching.id
      assert second.id == context.other.id
    end
  end

  test "collection callback messages survive calculation argument validation" do
    # query(:load, :array_matches?, ["message-invalid"])
    #   -> Label.apply_constraints_array -> {:error, "labels were rejected"}.
    # The calculation must not run: the public read error must retain the custom
    # callback's message instead of replacing it with a generic validation error.

    query = query(:load, :array_matches?, ["message-invalid"])
    assert {:error, error} = Ash.read(query)
    assert Exception.message(error) =~ "labels were rejected"
  end
end
