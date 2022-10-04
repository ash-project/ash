defmodule Ash.Test.Resource.ResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, data_layer: Ash.DataLayer.Ets

        actions do
          defaults [:create, :read, :update, :destroy]
        end

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end
    end
  end

  defmodule Concat do
    # An example concatenation calculation, that accepts the delimiter as an argument
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

  defmodule Post do
    @moduledoc false
    use Ash.Resource

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "it returns the correct error when doing a read with no data layer setup" do
    Post
    |> Ash.Changeset.new(%{name: "foo"})
    |> Api.create()

    {_, error} = Api.read(Post)
    [%Ash.Error.SimpleDataLayer.NoDataProvided{message: message} | _] = error.errors
    assert message != nil
  end

  test "fails if there are multiple fields that share the same name" do
    assert_raise(
      Spark.Error.DslError,
      ~r/There are 4 fields\(attributes, calculations, aggregates, and relationships\) that share the name `foobar`/,
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
