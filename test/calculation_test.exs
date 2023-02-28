defmodule Ash.Test.CalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

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

  defmodule NameWithUsersName do
    # Don't do this kind of thing in real life
    use Ash.Calculation

    def load(_, _, _) do
      [:full_name]
    end

    def calculate(records, _opts, %{actor: actor}) do
      Enum.map(records, fn record ->
        record.full_name <> " " <> actor.first_name <> " " <> actor.last_name
      end)
    end
  end

  defmodule ConcatWithLoad do
    # An example concatenation calculation, that accepts the delimiter as an argument
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

    def select(_query, opts, _) do
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

    def load(_query, _opts, _) do
      [best_friend: :full_name]
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        record.best_friend && record.best_friend.full_name
      end)
    end
  end

  defmodule NamesOfBestFriendsOfMe do
    use Ash.Calculation

    def load(_query, _opts, args) do
      if args[:only_special] do
        query =
          __MODULE__.User
          |> Ash.Query.filter(special == true)
          |> Ash.Query.ensure_selected(:full_name)

        [best_friends_of_me: query]
      else
        [best_friends_of_me: :full_name]
      end
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        record.best_friends_of_me
        |> Enum.map(& &1.full_name)
        |> Enum.sort()
        |> Enum.join(" - ")
      end)
    end
  end

  defmodule BestFriendsFirstNamePlusStuff do
    use Ash.Calculation

    def load(_query, _opts, _) do
      [:best_friends_first_name]
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        record.best_friends_first_name && record.best_friends_first_name <> " stuff"
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

      read :paginated do
        pagination do
          keyset? true
          default_limit 10
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :first_name, :string
      attribute :last_name, :string
      attribute :special, :boolean
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

      calculate :best_friends_first_name_plus_stuff,
                :string,
                BestFriendsFirstNamePlusStuff

      calculate :full_name_plus_full_name,
                :string,
                {ConcatWithLoad, keys: [:full_name, :full_name]}

      calculate :full_name_plus_full_name_plus_full_name,
                :string,
                {ConcatWithLoad, keys: [:full_name, :full_name_plus_full_name]}

      calculate :slug, :string, expr(full_name <> "123"), load: [:full_name]

      calculate :expr_full_name, :string, expr(first_name <> " " <> last_name)

      calculate :string_join_full_name,
                :string,
                expr(string_join([first_name, last_name], " "))

      calculate :best_friends_name, :string, BestFriendsName

      calculate :names_of_best_friends_of_me, :string, NamesOfBestFriendsOfMe
      calculate :name_with_users_name, :string, NameWithUsersName

      calculate :full_name_with_salutation,
                :string,
                expr(^arg(:salutation) <> " " <> conditional_full_name) do
        argument :salutation, :string, allow_nil?: false
        load [:conditional_full_name]
      end

      calculate :conditional_full_name,
                :string,
                expr(
                  if(
                    not is_nil(first_name) and not is_nil(last_name),
                    first_name <> " " <> last_name,
                    "(none)"
                  )
                )

      calculate :conditional_full_name_block,
                :string,
                expr(
                  if not is_nil(first_name) and not is_nil(last_name) do
                    first_name <> " " <> last_name
                  else
                    "(none)"
                  end
                )

      calculate :conditional_full_name_cond,
                :string,
                expr(
                  cond do
                    not is_nil(first_name) and not is_nil(last_name) ->
                      first_name <> " " <> last_name

                    not is_nil(first_name) ->
                      first_name

                    true ->
                      "(none)"
                  end
                )
    end

    aggregates do
      first :best_friends_first_name, :best_friend, :first_name
    end

    relationships do
      belongs_to :best_friend, __MODULE__

      has_many :best_friends_of_me, __MODULE__ do
        destination_attribute :best_friend_id
      end
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
      |> Ash.Changeset.manage_relationship(:best_friend, user1, type: :append_and_remove)
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

  test "loads dependencies", %{user1: user} do
    assert %{slug: "zach daniel123"} = Api.load!(user, [:slug])
  end

  test "calculations can access the actor", %{user1: user1, user2: user2} do
    assert %{
             name_with_users_name: "zach daniel brian cranston"
           } =
             user1
             |> Api.load!(:name_with_users_name, actor: user2)
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

  test "it loads anything specified by the load callback, even when nested" do
    full_names =
      User
      |> Ash.Query.load(:full_name_plus_full_name_plus_full_name)
      |> Api.read!()
      |> Enum.map(& &1.full_name_plus_full_name_plus_full_name)
      |> Enum.sort()

    assert full_names == [
             "brian cranston brian cranston brian cranston",
             "zach daniel zach daniel zach daniel"
           ]
  end

  test "it doesn't reload anything specified by the load callback if its already been loaded when using `lazy?: true`" do
    full_names =
      User
      |> Ash.Query.load(:full_name_plus_full_name)
      |> Api.read!()
      |> Enum.map(&%{&1 | full_name: &1.full_name <> " more"})
      |> Api.load!(:full_name_plus_full_name, lazy?: true)
      |> Enum.map(& &1.full_name_plus_full_name)
      |> Enum.sort()

    assert full_names == [
             "brian cranston more brian cranston more",
             "zach daniel more zach daniel more"
           ]
  end

  test "it reloads anything specified by the load callback if its already been loaded when using `lazy?: false`" do
    full_names =
      User
      |> Ash.Query.load(:full_name_plus_full_name)
      |> Api.read!()
      |> Enum.map(&%{&1 | full_name: &1.full_name <> " more"})
      |> Api.load!(:full_name_plus_full_name)
      |> Enum.map(& &1.full_name_plus_full_name)
      |> Enum.sort()

    assert full_names == [
             "brian cranston brian cranston",
             "zach daniel zach daniel"
           ]
  end

  test "nested calculations are loaded if necessary" do
    best_friends_names =
      User
      |> Ash.Query.load([:best_friends_name])
      |> Api.read!()
      |> Enum.map(& &1.best_friends_name)
      |> Enum.sort()

    assert best_friends_names == [nil, "zach daniel"]
  end

  test "nested aggregates are loaded if necessary" do
    best_friends_first_names_plus_stuff =
      User
      |> Ash.Query.load([:best_friends_first_name_plus_stuff])
      |> Api.read!()
      |> Enum.map(& &1.best_friends_first_name_plus_stuff)
      |> Enum.sort()

    assert best_friends_first_names_plus_stuff == [nil, "zach stuff"]
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

  test "can sort on calculation in paginated read" do
    full_names =
      User
      |> Ash.Query.for_read(:paginated)
      |> Ash.Query.load(full_name_with_salutation: [salutation: "Mr"])
      |> Ash.Query.sort(full_name_with_salutation: {:asc, %{salutation: "Mr"}})
      |> Api.read!()
      |> Map.get(:results)
      |> Enum.map(& &1.full_name_with_salutation)

    assert full_names == ["Mr brian cranston", "Mr zach daniel"]
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

  test "expression based calculations can handle lists of fields" do
    User
    |> Ash.Changeset.new(%{first_name: "bob"})
    |> Api.create!()

    full_names =
      User
      |> Ash.Query.load(:string_join_full_name)
      |> Api.read!()
      |> Enum.map(& &1.string_join_full_name)
      |> Enum.sort()

    assert full_names == ["bob", "brian cranston", "zach daniel"]
  end

  # test "loading calculations with different relationship dependencies won't collide", %{
  #   user1: %{id: user1_id} = user1
  # } do
  #   user3 =
  #     User
  #     |> Ash.Changeset.new(%{first_name: "chidi", last_name: "anagonye", special: true})
  #     |> Ash.Changeset.manage_relationship(:best_friend, user1, type: :append_and_remove)
  #     |> Api.create!()

  #   assert %{
  #            calculations: %{
  #              names_of_best_friends_of_me: "brian cranston - chidi anagonye",
  #              names_of_special_best_friends_of_me: "chidi anagonye"
  #            }
  #          } =
  #            User
  #            |> Ash.Query.filter(id == ^user1_id)
  #            |> Ash.Query.load_calculation_as(
  #              :names_of_best_friends_of_me,
  #              :names_of_special_best_friends_of_me,
  #              %{
  #                special: true
  #              }
  #            )
  #            |> Ash.Query.load_calculation_as(
  #              :names_of_best_friends_of_me,
  #              :names_of_best_friends_of_me
  #            )
  #            |> Api.read_one!()
  # end
end
