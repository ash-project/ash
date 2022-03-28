defmodule Ash.Test.Resource.ResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end
    end
  end

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

  test "fails if there are multiple fields that share the same name" do
    assert_raise(
      Ash.Error.Dsl.DslError,
      "[Ash.Test.Resource.ResourceTest.Post]\n There are 4 fields(attributes, calculations, aggregates, and relationships) that share the name `foobar` defined in Ash.Test.Resource.ResourceTest.Post\n",
      fn ->
        defposts do
          attributes do
            attribute(:foobar, :string)
          end

          relationships do
            belongs_to(:foobar, Foobar)
          end

          calculations do
            calculate :foobar, {Concat, keys: [:foo, :bar]} do
            end
          end

          aggregates do
            count :foobar, :baz
          end
        end
      end
    )
  end
end
