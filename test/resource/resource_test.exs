defmodule Ash.Test.Resource.ResourceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        actions do
          default_accept :*
          defaults [:read, :destroy, create: :*, update: :*]
        end

        attributes do
          uuid_primary_key :id
        end

        unquote(body)
      end

      alias unquote(module), as: Post
    end
  end

  defmodule Concat do
    # An example concatenation calculation, that accepts the delimiter as an argument
    use Ash.Resource.Calculation

    def init(opts) do
      if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
        {:ok, opts}
      else
        {:error, "Expected a `keys` option for which keys to concat"}
      end
    end

    def calculate(records, opts, %Ash.Resource.Calculation.Context{
          arguments: %{separator: separator}
        }) do
      Enum.map(records, fn record ->
        Enum.map_join(opts[:keys], separator, fn key ->
          to_string(Map.get(record, key))
        end)
      end)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  test "it returns the correct error when doing a read with no data layer setup" do
    Post
    |> Ash.Changeset.for_create(:create, %{name: "foo"})
    |> Ash.create()

    {_, error} = Ash.read(Post)
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
            attribute(:foobar, :string, public?: true)
          end

          relationships do
            belongs_to :foobar, Foobar do
              public?(true)
            end
          end

          calculations do
            calculate :foobar, :integer, {Concat, keys: [:foo, :bar]} do
              public?(true)
            end
          end

          aggregates do
            count :foobar, :baz do
              public? true
            end
          end
        end
      end
    )
  end

  test "relationships can be loaded" do
    defmodule Leg do
      @moduledoc false
      use Ash.Resource,
        domain: nil,
        data_layer: Ash.DataLayer.Ets,
        validate_domain_inclusion?: false

      attributes do
        uuid_primary_key :id
        attribute :side, :string, allow_nil?: false, public?: true
      end

      relationships do
        belongs_to :pants, Ash.Test.Resource.ResourceTest.Pants do
          attribute_writable? true
          public? true
        end
      end

      actions do
        defaults [:create, :read]
        default_accept :*
      end
    end

    defmodule Pants do
      @moduledoc false
      use Ash.Resource,
        domain: nil,
        data_layer: Ash.DataLayer.Ets,
        validate_domain_inclusion?: false

      attributes do
        uuid_primary_key :id
      end

      relationships do
        has_many :legs, Leg
      end

      actions do
        defaults [:create, :read]
        default_accept :*
      end
    end

    defmodule Clothing do
      @moduledoc false
      use Ash.Domain, validate_config_inclusion?: false

      resources do
        allow_unregistered? true
      end
    end

    pants =
      Pants
      |> Ash.Changeset.for_create(:create, %{}, domain: Clothing)
      |> Ash.create!(domain: Clothing)

    [left, _right] =
      ~w[left right]
      |> Enum.map(fn side ->
        Leg
        |> Ash.Changeset.for_create(:create, %{pants_id: pants.id, side: side}, domain: Clothing)
        |> Ash.create!(domain: Clothing)
      end)

    Ash.load!(left, [pants: [:legs]], domain: Clothing)
  end
end
