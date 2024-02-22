defmodule Ash.Test.CalculationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  alias Ash.Test.AnyApi, as: Api

  defmodule FriendLink do
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    relationships do
      belongs_to :source, Ash.Test.CalculationTest.User do
        allow_nil?(false)
        primary_key?(true)
      end

      belongs_to :target, Ash.Test.CalculationTest.User do
        allow_nil?(false)
        primary_key?(true)
      end
    end
  end

  defmodule FullNameWithSelect do
    use Ash.Calculation

    def select(_, _, _) do
      [:first_name, :last_name]
    end

    def calculate(records, _, _) do
      Enum.map(records, fn record ->
        "#{record.first_name} #{record.last_name}"
      end)
    end
  end

  defmodule Metadata do
    use Ash.Calculation

    def calculate(records, _opts, _context) do
      Enum.map(records, &Ash.Resource.get_metadata(&1, :example_metadata))
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
          Ash.Test.CalculationTest.User
          |> Ash.Query.filter(special == true)
          |> Ash.Query.load(:full_name)

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

  defmodule BestFriendsBestFriendsFirstName do
    use Ash.Calculation

    def load(_query, _opts, _) do
      [best_friend: :best_friends_first_name]
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        "bf: #{record.best_friend && record.best_friend.best_friends_first_name}"
      end)
    end
  end

  defmodule FullNamePlusFirstName do
    use Ash.Calculation

    def load(_, _, _) do
      [:full_name_with_select]
    end

    def select(_, _, _) do
      [:first_name]
    end

    def calculate(records, _, _) do
      Enum.map(records, fn record ->
        record.full_name_with_select <> " #{record.first_name}"
      end)
    end
  end

  defmodule FriendsNames do
    use Ash.Calculation

    def load(_query, _opts, _) do
      [
        friends:
          Ash.Test.CalculationTest.User
          |> Ash.Query.load([:full_name, :prefix])
          |> Ash.Query.limit(5)
      ]
    end

    def calculate(records, _opts, _) do
      Enum.map(records, fn record ->
        record.friends
        |> Enum.map_join(" | ", fn friend ->
          if friend.prefix do
            friend.prefix <> " " <> friend.full_name
          else
            friend.full_name
          end
        end)
      end)
    end
  end

  defmodule RoleHasUser do
    @moduledoc "never do this, this is done for testing only"

    use Ash.Calculation

    def load(_, _, _) do
      [:user]
    end

    def calculate(list, _opts, _context) do
      Enum.map(list, &(not is_nil(&1.user)))
    end
  end

  defmodule RolesHaveUser do
    use Ash.Calculation

    def load(_, _, _) do
      [roles: [:has_user]]
    end

    def calculate(list, _, _) do
      Enum.map(list, fn user ->
        Enum.all?(user.roles, & &1.has_user)
      end)
    end
  end

  defmodule Role do
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    actions do
      defaults([:create, :read, :update, :destroy])

      read :by_user_name do
        argument :user_name, :string, allow_nil?: false
        filter expr(user_name == ^arg(:user_name))
      end
    end

    ets do
      private?(true)
    end

    aggregates do
      first :user_is_active, :user, :is_active do
        default(false)
      end

      list :names, :user, :first_name
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:name, :string)

      attribute :is_active, :boolean do
        allow_nil?(false)
        default(false)
      end
    end

    calculations do
      calculate :active, :boolean do
        calculation(expr(is_active && user_is_active))
        load([:user_is_active])
      end

      calculate :active_elixir, :boolean do
        calculation fn record, _ ->
          record.is_active && record.user_is_active
        end

        load [:is_active, :user_is_active]
      end

      calculate :has_user, :boolean, RoleHasUser

      calculate :user_is_active_with_calc, :boolean, expr(user.is_active || false)

      calculate :user_name,
                :string,
                expr(
                  if is_nil(^actor(:id)) do
                    "no actor"
                  else
                    at(names, 0)
                  end
                )
    end

    relationships do
      belongs_to(:user, Ash.Test.CalculationTest.User) do
        attribute_writable?(true)
      end
    end
  end

  defmodule Bio do
    use Ash.Resource, api: Api, data_layer: :embedded

    attributes do
      attribute :greeting, :string
    end

    calculations do
      calculate :say_hello, :string, expr(greeting <> " " <> ^arg(:to)) do
        argument :to, :string, allow_nil?: false
      end
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults([:create, :read, :update, :destroy])

      read :paginated do
        pagination do
          keyset?(true)
          default_limit(10)
        end
      end
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:first_name, :string)
      attribute(:last_name, :string)
      attribute(:prefix, :string)
      attribute(:special, :boolean)
      attribute(:is_active, :boolean)
      attribute(:bio, Bio)
    end

    changes do
      change fn changeset, _ ->
        if changeset.action_type == :create do
          Ash.Changeset.after_action(changeset, fn changeset, result ->
            {:ok, Ash.Resource.put_metadata(result, :example_metadata, "example metadata")}
          end)
        else
          changeset
        end
      end
    end

    calculations do
      calculate :say_hello_to_fred, :string do
        calculation fn record, _ ->
          record.bio.say_hello
        end

        load bio: [say_hello: %{to: "Fred"}]
      end

      calculate :bio_greeting, :string, expr(bio[:greeting])

      calculate :say_hello_to_fred_expr, :string, expr(record.bio.say_hello(to: "Fred"))

      calculate :say_hello_to_george_expr, :string, expr(record.bio.say_hello(to: "George"))

      calculate :say_hello_to_george, :string do
        calculation fn record, _ ->
          record.bio.say_hello
        end

        load bio: [say_hello: %{to: "George"}]
      end

      calculate :example_metadata, :string, Metadata

      calculate :metadata_plus_metadata, :string, concat([:example_metadata, :example_metadata])

      calculate(:active, :boolean, expr(is_active))

      calculate :full_name, :string, {Concat, keys: [:first_name, :last_name]} do
        select([:first_name, :last_name])
        # We currently need to use the [allow_empty?: true, trim?: false] constraints here.
        # As it's an empty string, the separator would otherwise be trimmed and set to `nil`.
        argument(:separator, :string,
          default: " ",
          constraints: [allow_empty?: true, trim?: false]
        )
      end

      calculate :full_name_with_select, :string, FullNameWithSelect do
        # We currently need to use the [allow_empty?: true, trim?: false] constraints here.
        # As it's an empty string, the separator would otherwise be trimmed and set to `nil`.
        argument(:separator, :string,
          default: " ",
          constraints: [allow_empty?: true, trim?: false]
        )
      end

      calculate :full_name_with_select_plus_something,
                :string,
                expr(full_name_with_select <> "_something")

      calculate(:best_friends_best_friends_first_name, :string, BestFriendsBestFriendsFirstName)

      calculate(
        :best_friends_first_name_plus_stuff,
        :string,
        BestFriendsFirstNamePlusStuff
      )

      calculate(:full_name_plus_first_name, :string, FullNamePlusFirstName)

      calculate(
        :full_name_plus_full_name,
        :string,
        {ConcatWithLoad, keys: [:full_name, :full_name]}
      )

      calculate(
        :full_name_plus_full_name_plus_full_name,
        :string,
        {ConcatWithLoad, keys: [:full_name, :full_name_plus_full_name]}
      )

      calculate(:slug, :string, expr(full_name <> "123"))
      calculate(:friends_names, :string, FriendsNames)

      calculate(:expr_full_name, :string, expr(first_name <> " " <> last_name))

      calculate(
        :string_join_full_name,
        :string,
        expr(string_join([first_name, last_name], " "))
      )

      calculate(
        :string_join_full_name_ci,
        :ci_string,
        expr(string_join([first_name, last_name], ~i" "))
      )

      calculate(:best_friends_name, :string, BestFriendsName)

      calculate :names_of_best_friends_of_me, :string, NamesOfBestFriendsOfMe do
        argument(:only_special, :boolean, default: false)
      end

      calculate(:name_with_users_name, :string, NameWithUsersName)

      calculate :full_name_with_salutation,
                :string,
                expr(^arg(:salutation) <> " " <> conditional_full_name) do
        argument(:salutation, :string, allow_nil?: false)
        load([:conditional_full_name])
      end

      calculate(
        :conditional_full_name,
        :string,
        expr(
          if(
            not is_nil(first_name) and not is_nil(last_name),
            first_name <> " " <> last_name,
            "(none)"
          )
        )
      )

      calculate(
        :conditional_full_name_block,
        :string,
        expr(
          if not is_nil(first_name) and not is_nil(last_name) do
            first_name <> " " <> last_name
          else
            "(none)"
          end
        )
      )

      calculate(
        :conditional_full_name_cond,
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
      )

      calculate :role_user_name_from_agg, :string, expr(role_user_name)
      calculate :roles_have_user, :boolean, RolesHaveUser
    end

    aggregates do
      first(:best_friends_first_name, :best_friend, :first_name)

      first(:role_user_name, :roles, :user_name)
    end

    relationships do
      belongs_to(:best_friend, __MODULE__)

      has_many :best_friends_of_me, __MODULE__ do
        destination_attribute(:best_friend_id)
      end

      has_many :roles, Ash.Test.CalculationTest.Role

      many_to_many :friends, __MODULE__ do
        through(FriendLink)
        destination_attribute_on_join_resource(:target_id)
        source_attribute_on_join_resource(:source_id)
      end
    end
  end

  defmodule ActorActive do
    use Ash.Calculation

    def load(_, _, _) do
      [user: [:active], role: [:active]]
    end

    def calculate(records, _, _) do
      Enum.map(records, fn actor ->
        if actor.role do
          actor.role.active
        else
          actor.user.active
        end
      end)
    end
  end

  defmodule Actor do
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets

    actions do
      defaults([:create, :read, :update, :destroy])
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
    end

    calculations do
      calculate(:active, :boolean, ActorActive)
    end

    relationships do
      belongs_to :user, User do
        attribute_writable?(true)
      end

      belongs_to :role, Role do
        attribute_writable?(true)
      end
    end
  end

  setup do
    user1 =
      User
      |> Ash.Changeset.new(%{first_name: "zach", last_name: "daniel", bio: %{greeting: "Yo! "}})
      |> Api.create!()

    admin_role =
      Role
      |> Ash.Changeset.for_create(:create, %{name: "admin"})
      |> Api.create!()

    normie_role =
      Role
      |> Ash.Changeset.for_create(:create, %{name: "normie"})
      |> Api.create!()

    actor1 =
      Actor
      |> Ash.Changeset.for_create(:create, %{user_id: user1.id, role_id: admin_role.id})
      |> Api.create!()

    user2 =
      User
      |> Ash.Changeset.new(%{first_name: "brian", last_name: "cranston"})
      |> Ash.Changeset.manage_relationship(:best_friend, user1, type: :append_and_remove)
      |> Ash.Changeset.manage_relationship(:friends, user1, type: :append_and_remove)
      |> Api.create!()

    actor2 =
      Actor
      |> Ash.Changeset.for_create(:create, %{user_id: user2.id, role_id: normie_role.id})
      |> Api.create!()

    %{
      user1: user1,
      user2: user2,
      admin_role: admin_role,
      normie_role: normie_role,
      actor1: actor1,
      actor2: actor2
    }
  end

  test "calculations can refer to `to_one` relationships in filters" do
    Role
    |> Ash.Query.filter(user_is_active_with_calc == true)
    |> Api.read!()
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

  test "calculations can depend on relationships directly" do
    Role
    |> Ash.Query.load(:has_user)
    |> Api.read!()
  end

  test "read actions can load calculations that use the actor", %{actor1: actor} do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "zach"})
      |> Api.create!()

    Role
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Api.create!()

    assert [%{user_name: "zach"}] =
             Role
             |> Ash.Query.for_read(:by_user_name, %{user_name: "zach"}, actor: actor)
             |> Ash.Query.load(:user_name)
             |> Api.read!()
  end

  test "calculations that depend on relationships directly can be loaded from elsewhere", %{
    user1: user1,
    admin_role: role
  } do
    role
    |> Ash.Changeset.for_update(:update)
    |> Ash.Changeset.manage_relationship(:user, user1, type: :append)
    |> Api.update!()

    User
    |> Ash.Query.load(roles: :has_user)
    |> Api.read!()
  end

  test "calculations can depend on calculations that depend on relationships directly", %{
    user1: user1,
    admin_role: role
  } do
    role
    |> Ash.Changeset.for_update(:update, %{})
    |> Ash.Changeset.manage_relationship(:user, user1, type: :append)
    |> Api.update!()

    User
    |> Ash.Query.load(:roles_have_user)
    |> Api.read!()
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

  test "it reloads anything specified by the load callback if its already been loaded when using `lazy?: false`" do
    full_names =
      User
      |> Ash.Query.load([:full_name, :full_name_plus_full_name])
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

    ci_full_names =
      User
      |> Ash.Query.load(:string_join_full_name_ci)
      |> Api.read!()
      |> Enum.map(&Ash.CiString.value(&1.string_join_full_name_ci))
      |> Enum.sort()

    assert ci_full_names == ["bob", "brian cranston", "zach daniel"]
  end

  test "simple expression calculations will be run in memory" do
    user =
      User
      |> Ash.Changeset.new(%{first_name: "bob", last_name: "sagat"})
      |> Api.create!()

    assert "george sagat" ==
             user
             |> Map.put(:first_name, "george")
             |> Api.load!(:string_join_full_name)
             |> Map.get(:string_join_full_name)
  end

  test "loading calculations with different relationship dependencies won't collide", %{
    user1: %{id: user1_id} = user1
  } do
    User
    |> Ash.Changeset.new(%{first_name: "chidi", last_name: "anagonye", special: true})
    |> Ash.Changeset.manage_relationship(:best_friend, user1, type: :append_and_remove)
    |> Api.create!()

    assert %{
             calculations: %{
               names_of_best_friends_of_me: "brian cranston - chidi anagonye",
               names_of_special_best_friends_of_me: "chidi anagonye"
             }
           } =
             User
             |> Ash.Query.filter(id == ^user1_id)
             |> Ash.Query.load_calculation_as(
               :names_of_best_friends_of_me,
               :names_of_special_best_friends_of_me,
               %{
                 only_special: true
               }
             )
             |> Ash.Query.load_calculation_as(
               :names_of_best_friends_of_me,
               :names_of_best_friends_of_me
             )
             |> Api.read_one!()
  end

  test "loading calculations with different relationship dependencies won't collide, when manually loading one of the deps",
       %{
         user1: %{id: user1_id} = user1
       } do
    User
    |> Ash.Changeset.new(%{first_name: "chidi", last_name: "anagonye", special: true})
    |> Ash.Changeset.manage_relationship(:best_friend, user1, type: :append_and_remove)
    |> Api.create!()

    assert %{
             calculations: %{
               names_of_best_friends_of_me: "brian cranston - chidi anagonye",
               names_of_special_best_friends_of_me: "chidi anagonye"
             }
           } =
             User
             |> Ash.Query.filter(id == ^user1_id)
             |> Ash.Query.load(:best_friends_of_me)
             |> Ash.Query.load_calculation_as(
               :names_of_best_friends_of_me,
               :names_of_special_best_friends_of_me,
               %{
                 only_special: true
               }
             )
             |> Ash.Query.load_calculation_as(
               :names_of_best_friends_of_me,
               :names_of_best_friends_of_me
             )
             |> Api.read_one!()
  end

  test "calculations that depend on many to many relationships will load", %{user2: user2} do
    assert %{
             friends_names: "zach daniel"
           } =
             User
             |> Ash.Query.filter(id == ^user2.id)
             |> Ash.Query.load(:friends_names)
             |> Api.read_one!()
  end

  test "when already loading a calculation's dependency it is used" do
    full_names =
      User
      |> Ash.Query.load([:full_name, :full_name_plus_full_name])
      |> Api.read!()
      |> Enum.map(& &1.full_name_plus_full_name)
      |> Enum.sort()

    assert full_names == ["brian cranston brian cranston", "zach daniel zach daniel"]
  end

  test "when already loading a nested calculation dependency, it is used" do
    best_friends_names =
      User
      |> Ash.Query.load([:best_friends_name, best_friend: [:full_name]])
      |> Api.read!()
      |> Enum.map(& &1.best_friends_name)
      |> Enum.sort()

    assert best_friends_names == [nil, "zach daniel"]
  end

  test "loading a nested aggregate works" do
    best_friends_best_friends_first_names =
      User
      |> Ash.Query.load(:best_friends_best_friends_first_name)
      |> Api.read!()
      |> Enum.map(& &1.best_friends_best_friends_first_name)
      |> Enum.sort()

    assert best_friends_best_friends_first_names == ["bf: ", "bf: "]
  end

  test "loading a nested aggregate works even when its already being loaded" do
    best_friends_best_friends_first_names =
      User
      |> Ash.Query.load([
        :best_friends_best_friends_first_name,
        best_friend: [best_friend: [:first_name]]
      ])
      |> Api.read!()
      |> Enum.map(& &1.best_friends_best_friends_first_name)
      |> Enum.sort()

    assert best_friends_best_friends_first_names == ["bf: ", "bf: "]
  end

  test "loading a calculation with selects that loads a calculation with selects works" do
    assert ["brian cranston brian", "zach daniel zach"] ==
             User
             |> Ash.Query.select([])
             |> Ash.Query.load([
               :full_name_plus_first_name
             ])
             |> Api.read!()
             |> Enum.map(& &1.full_name_plus_first_name)
             |> Enum.sort()
  end

  test "nested calculations that depend on aggregates work" do
    assert [false, false] =
             Actor
             |> Ash.Query.load(:active)
             |> Api.read!(authorize?: true)
             |> Enum.map(& &1.active)
  end

  test "aggregates using calculations pass actor to calculations", %{
    user1: user,
    actor1: actor,
    admin_role: role
  } do
    user1 = user

    role
    |> Ash.Changeset.for_update(:update, %{}, actor: actor)
    |> Ash.Changeset.manage_relationship(:user, user1, type: :append)
    |> Api.update!(actor: actor)

    roles = Role |> Ash.Query.load(:user_name) |> Api.read!(actor: actor)

    zach_role =
      Enum.find(roles, fn role ->
        role.user_name == "zach"
      end)

    assert zach_role.user_name == "zach"

    users =
      User
      |> Ash.Query.select([])
      |> Ash.Query.load([:role_user_name, :role_user_name_from_agg])
      |> Api.read!(actor: actor)

    zach_user =
      Enum.find(users, fn user ->
        user.id == zach_role.user_id
      end)

    assert zach_user.role_user_name == "zach"
    assert zach_user.role_user_name_from_agg == "zach"
  end

  test "invalid calculation arguments show errors in the query" do
    assert %{valid?: false} =
             User
             |> Ash.Query.load(full_name: %{separator: %{foo: :bar}})
  end

  test "calculation dependencies with conflicting load throughs still receive the appropriate values",
       %{user1: user1} do
    user1
    |> Api.load!([:say_hello_to_fred, :say_hello_to_george])
  end

  test "calculation dependencies can detect non-loaded nested values", %{user1: user1} do
    greeting =
      user1
      |> Map.update!(:bio, fn bio ->
        %{bio | greeting: "A new value it definitely wasn't before!"}
      end)
      |> Api.load!([:bio_greeting])
      |> Map.get(:bio_greeting)

    assert greeting == "A new value it definitely wasn't before!"

    greeting =
      user1
      |> Map.update!(:bio, fn bio ->
        %{bio | greeting: %Ash.NotLoaded{}}
      end)
      |> Api.load!([:bio_greeting])
      |> Map.get(:bio_greeting)

    assert greeting == "Yo!"
  end

  test "calculation dependencies are refetched if `reselect_all?` is `true`", %{user1: user1} do
    greeting =
      user1
      |> Map.update!(:bio, fn bio ->
        %{bio | greeting: "A new value it definitely wasn't before!"}
      end)
      |> Api.load!([:bio_greeting], reselect_all?: true)
      |> Map.get(:bio_greeting)

    assert greeting == "Yo!"
  end

  test "expression calculations that depend on runtime calculations work", %{user1: user1} do
    assert [%{full_name_with_select_plus_something: "zach daniel_something"}] =
             User
             |> Ash.Query.filter(id == ^user1.id)
             |> Ash.Query.load(:full_name_with_select_plus_something)
             |> Api.read!()
  end

  test "calculations that extract metadata will be loaded as a dependency of the concat calculation",
       %{user1: user1} do
    assert "example metadataexample metadata" ==
             user1
             |> Api.load!(:metadata_plus_metadata)
             |> Map.get(:metadata_plus_metadata)
  end

  test "metadata is persisted after an update", %{user1: user1} do
    assert %{example_metadata: "example metadata"} =
             user1.__metadata__

    assert %{example_metadata: "example metadata"} =
             user1
             |> Ash.Changeset.for_update(:update, %{first_name: "something new"})
             |> Api.update!()
             |> Map.get(:__metadata__)
  end
end
