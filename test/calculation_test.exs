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

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :read
      create :create
      update :update
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

      calculate :expr_full_name, :string, expr(first_name <> " " <> last_name)

      calculate :conditional_full_name,
                :string,
                expr(if(first_name and last_name, first_name <> " " <> last_name, "(none)"))
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(User)
    end
  end

  setup do
    User
    |> Ash.Changeset.new(%{first_name: "zach", last_name: "daniel"})
    |> Api.create!()

    User
    |> Ash.Changeset.new(%{first_name: "brian", last_name: "cranston"})
    |> Api.create!()

    :ok
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
end
