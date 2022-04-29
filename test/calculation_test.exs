defmodule Ash.Test.CalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Concat do
    # An example concatenation calculation, that accepts the delimeter as an argument
    use Ash.Calculation

    def init(opts) do
      if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
        {:ok, opts}
      else
        {:error, "Expected a `keys` option for which keys to concat"}
      end
    end

    def calculate(records, opts, %{separator: separator}) do
      Enum.map(records, fn record ->
        Enum.map_join(opts[:keys], separator, fn key ->
          to_string(Map.get(record, key))
        end)
      end)
    end
  end

  defmodule ConcatWithLoad do
    # An example concatenation calculation, that accepts the delimeter as an argument
    use Ash.Calculation

    def init(opts) do
      if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
        {:ok, opts}
      else
        {:error, "Expected a `keys` option for which keys to concat"}
      end
    end

    def load(_query, opts, _) do
      opts[:keys]
    end

    def calculate(records, opts, _) do
      Enum.map(records, fn record ->
        Enum.map_join(opts[:keys], " ", fn key ->
          to_string(Map.get(record, key))
        end)
      end)
    end
  end

  defmodule BestFriendsName do
    use Ash.Calculation

    def load(_query, opts, _) do
      [best_friend: :full_name]
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        record.best_friend && record.best_friend.full_name
      end)
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string
      attribute :last_name, :string
    end

    calculations do
      calculate :full_name, :string, {Concat, keys: [:first_name, :last_name]} do
        select [:first_name, :last_name]
        # We currently need to use the [allow_empty?: true, trim?: false] constraints here.
        # As it's an empty string, the separator would otherwise be trimmed and set to `nil`.
        argument :separator, :string,
          default: " ",
          constraints: [allow_empty?: true, trim?: false]
      end

      calculate :full_name_plus_full_name,
                :string,
                {ConcatWithLoad, keys: [:full_name, :full_name]}

      calculate :expr_full_name, :string, expr(first_name <> " " <> last_name) do
        allow_async? true
      end

      calculate :best_friends_name, :string, BestFriendsName

      calculate :conditional_full_name,
                :string,
                expr(if(first_name and last_name, first_name <> " " <> last_name, "(none)"))

      calculate :conditional_full_name_block,
                :string,
                expr(
                  if first_name and last_name do
                    first_name <> " " <> last_name
                  else
                    "(none)"
                  end
                )

      calculate :conditional_full_name_cond,
                :string,
                expr(
                  cond do
                    first_name and last_name ->
                      first_name <> " " <> last_name

                    first_name ->
                      first_name

                    true ->
                      "(none)"
                  end
                )
    end

    relationships do
      belongs_to :best_friend, __MODULE__
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(User)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  setup do
    user1 =
      User
      |> Ash.Changeset.new(%{first_name: "zach", last_name: "daniel"})
      |> Api.create!()

    user2 =
      User
      |> Ash.Changeset.new(%{first_name: "brian", last_name: "cranston"})
      |> Ash.Changeset.replace_relationship(:best_friend, user1)
      |> Api.create!()

    %{user1: user1, user2: user2}
  end

  test "it uses default arguments" do
    full_names =
      User
      |> Ash.Query.load(:full_name)
      |> Api.read!()
      |> Enum.map(& &1.full_name)
      |> Enum.sort()

    assert full_names == ["brian cranston", "zach daniel"]
  end

  test "it loads anything specified by the load callback" do
    full_names =
      User
      |> Ash.Query.load(:full_name_plus_full_name)
      |> Api.read!()
      |> Enum.map(& &1.full_name_plus_full_name)
      |> Enum.sort()

    assert full_names == ["brian cranston brian cranston", "zach daniel zach daniel"]
  end

  test "nested calculations are loaded if necessary" do
    best_friends_names =
      User
      |> Ash.Query.load(:best_friends_name)
      |> Api.read!()
      |> Enum.map(& &1.best_friends_name)
      |> Enum.sort()

    assert best_friends_names == [nil, "zach daniel"]
  end

  test "arguments can be supplied" do
    full_names =
      User
      |> Ash.Query.load(full_name: %{separator: " - "})
      |> Api.read!()
      |> Enum.map(& &1.full_name)
      |> Enum.sort()

    assert full_names == ["brian - cranston", "zach - daniel"]
  end

  test "fields are selected if necessary for the calculation" do
    full_names =
      User
      |> Ash.Query.select(:first_name)
      |> Ash.Query.load(full_name: %{separator: " - "})
      |> Api.read!()
      |> Enum.map(& &1.full_name)
      |> Enum.sort()

    assert full_names == ["brian - cranston", "zach - daniel"]
  end

  test "custom calculations can be added to a query" do
    full_names =
      User
      |> Ash.Query.calculate(:full_name, {Concat, keys: [:first_name, :last_name]}, :string, %{
        separator: " \o.o/ "
      })
      |> Api.read!()
      |> Enum.map(& &1.calculations.full_name)
      |> Enum.sort()

    assert full_names == ["brian \o.o/ cranston", "zach \o.o/ daniel"]
  end

  test "expression based calculations are resolved via evaluating the expression" do
    full_names =
      User
      |> Ash.Query.load(:expr_full_name)
      |> Api.read!()
      |> Enum.map(& &1.expr_full_name)
      |> Enum.sort()

    assert full_names == ["brian cranston", "zach daniel"]
  end

  test "expression based calculations can be sorted on" do
    full_names =
      User
      |> Ash.Query.load(:expr_full_name)
      |> Ash.Query.sort(:expr_full_name)
      |> Api.read!()
      |> Enum.map(& &1.expr_full_name)

    assert full_names == ["brian cranston", "zach daniel"]

    full_names =
      User
      |> Ash.Query.load(:expr_full_name)
      |> Ash.Query.sort(expr_full_name: :desc)
      |> Api.read!()
      |> Enum.map(& &1.expr_full_name)

    assert full_names == ["zach daniel", "brian cranston"]
  end

  test "the `if` calculation resolves the first expr when true, and the second when false" do
    User
    |> Ash.Changeset.new(%{first_name: "bob"})
    |> Api.create!()

    full_names =
      User
      |> Ash.Query.load(:conditional_full_name)
      |> Api.read!()
      |> Enum.map(& &1.conditional_full_name)
      |> Enum.sort()

    assert full_names == ["(none)", "brian cranston", "zach daniel"]
  end

  test "the `if` calculation can use the `do` style syntax" do
    User
    |> Ash.Changeset.new(%{first_name: "bob"})
    |> Api.create!()

    full_names =
      User
      |> Ash.Query.load(:conditional_full_name_block)
      |> Api.read!()
      |> Enum.map(& &1.conditional_full_name_block)
      |> Enum.sort()

    assert full_names == ["(none)", "brian cranston", "zach daniel"]
  end

  test "the `if` calculation can use the `cond` style syntax" do
    User
    |> Ash.Changeset.new(%{first_name: "bob"})
    |> Api.create!()

    full_names =
      User
      |> Ash.Query.load(:conditional_full_name_cond)
      |> Api.read!()
      |> Enum.map(& &1.conditional_full_name_cond)
      |> Enum.sort()

    assert full_names == ["bob", "brian cranston", "zach daniel"]
  end
end
