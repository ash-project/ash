defmodule Ash.Test.CalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Concat do
    # An example concatenation calculation, that accepts the delimeter as an argument
    use Ash.Calculation, type: :string

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
      read :default
      create :default
      update :default
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string
      attribute :last_name, :string
    end

    calculations do
      calculate :full_name, :string, {Concat, keys: [:first_name, :last_name]} do
        # You currently need to use the [allow_empty?: true] constraint here.
        # As it's an empty string, your separator would otherwise be set to `nil`.
        argument :separator, :string, default: " ", constraints: [allow_empty?: true]
      end
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

  test "custom calculations can be added to a query" do
    full_names =
      User
      |> Ash.Query.calculate(:full_name, {Concat, keys: [:first_name, :last_name]}, %{
        separator: " \o.o/ "
      })
      |> Api.read!()
      |> Enum.map(& &1.calculations.full_name)
      |> Enum.sort()

    assert full_names == ["brian \o.o/ cranston", "zach \o.o/ daniel"]
  end
end
