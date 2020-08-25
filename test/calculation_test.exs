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
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :first_name, :string
      attribute :last_name, :string
    end

    calculations do
      calculate :full_name, {Concat, keys: [:first_name, :last_name]} do
        argument :separator, :string, default: {:constant, " "}
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
end
