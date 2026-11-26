# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.BehaviourReturnValidationTest do
  @moduledoc """
  Verifies that behaviour wrappers raise Ash.Error.Framework.InvalidReturnType
  with a clear message when an implementation returns an invalid value.
  """
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  describe "Ash.Resource.Change" do
    defmodule ChangeReturnsWrong do
      use Ash.Resource.Change

      def change(_changeset, _opts, _context) do
        :wrong
      end
    end

    defmodule ChangeResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]

        create :other_action do
          # no change - we use this only to build a changeset
        end

        create :create_action do
          change ChangeReturnsWrong
        end
      end
    end

    test "raises InvalidReturnType when change returns wrong shape" do
      # Use other_action so building the changeset does not run ChangeReturnsWrong
      changeset = Ash.Changeset.for_create(ChangeResource, :other_action)
      context = struct(Ash.Resource.Change.Context, %{})

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Resource.Change.change(ChangeReturnsWrong, changeset, [], context)
      end
    end

    test "error message includes callback name and expected return type" do
      changeset = Ash.Changeset.for_create(ChangeResource, :other_action)
      context = struct(Ash.Resource.Change.Context, %{})

      try do
        Ash.Resource.Change.change(ChangeReturnsWrong, changeset, [], context)
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "change/3", "message should include callback name: #{message}"
          assert message =~ "ChangeReturnsWrong" or message =~ "change"

          assert message =~ "Ash.Changeset" or message =~ "changeset",
                 "message should describe expected return: #{message}"
      end
    end
  end

  describe "Ash.Resource.Validation" do
    defmodule ValidationReturnsWrong do
      use Ash.Resource.Validation

      def validate(_changeset, _opts, _context) do
        :wrong
      end
    end

    defmodule ValidationResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]

        create :other_action do
          # no validation - we use this only to build a changeset
        end

        create :create_action do
          validate ValidationReturnsWrong
        end
      end
    end

    test "raises InvalidReturnType when validate returns wrong shape" do
      # Use other_action so building the changeset does not run ValidationReturnsWrong
      changeset = Ash.Changeset.for_create(ValidationResource, :other_action)
      context = struct(Ash.Resource.Validation.Context, %{})

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Resource.Validation.validate(
          ValidationReturnsWrong,
          changeset,
          [],
          context
        )
      end
    end

    test "error message includes callback name and expected return types" do
      changeset = Ash.Changeset.for_create(ValidationResource, :other_action)
      context = struct(Ash.Resource.Validation.Context, %{})

      try do
        Ash.Resource.Validation.validate(
          ValidationReturnsWrong,
          changeset,
          [],
          context
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "validate", "message should include callback name: #{message}"
          assert message =~ "ValidationReturnsWrong" or message =~ "validate"

          assert (message =~ ":ok" or message =~ "{:error") and
                   (message =~ "expected" or message =~ "expects"),
                 "message should describe expected return types: #{message}"
      end
    end
  end

  describe "Ash.Authorizer" do
    defmodule AuthorizerReturnsWrongInitialState do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain) do
        :wrong
      end

      def strict_check_context(_state), do: [:query]
      def strict_check(_state, _context), do: {:authorized, %{}}
      def check_context(_state), do: [:query]
      def check(_state, _context), do: :authorized
    end

    defmodule AuthorizerResource do
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
      end

      actions do
        default_accept :*
        defaults [:read]
      end
    end

    defmodule AuthorizerWithProtectedFields do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain), do: %{}
      def strict_check_context(_state), do: []
      def strict_check(state, _context), do: {:authorized, state}
      def check_context(_state), do: []
      def check(_state, _context), do: :authorized
      def protected_fields(_resource), do: [:name, :email, :name]
    end

    defmodule AuthorizerWithoutProtectedFields do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain), do: %{}
      def strict_check_context(_state), do: []
      def strict_check(state, _context), do: {:authorized, state}
      def check_context(_state), do: []
      def check(_state, _context), do: :authorized
    end

    defmodule AuthorizerReturnsWrongProtectedFields do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain), do: %{}
      def strict_check_context(_state), do: []
      def strict_check(state, _context), do: {:authorized, state}
      def check_context(_state), do: []
      def check(_state, _context), do: :authorized
      def protected_fields(_resource), do: [:name, "email"]
    end

    defmodule ProtectedFieldsResource do
      use Ash.Resource,
        domain: Domain,
        data_layer: Ash.DataLayer.Ets,
        authorizers: [AuthorizerWithProtectedFields, AuthorizerWithoutProtectedFields]

      attributes do
        uuid_primary_key :id
        attribute :name, :string, public?: true
        attribute :email, :string, public?: true
      end

      actions do
        defaults [:read]
      end
    end

    test "raises InvalidReturnType when initial_state returns wrong shape" do
      action = %Ash.Resource.Actions.Action{name: :read, type: :read}

      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Authorizer.initial_state(
          AuthorizerReturnsWrongInitialState,
          nil,
          AuthorizerResource,
          action,
          Domain
        )
      end
    end

    test "error message includes callback name and expected return type" do
      action = %Ash.Resource.Actions.Action{name: :read, type: :read}

      try do
        Ash.Authorizer.initial_state(
          AuthorizerReturnsWrongInitialState,
          nil,
          AuthorizerResource,
          action,
          Domain
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)

          assert message =~ "initial_state/4",
                 "message should include callback name: #{message}"

          assert message =~ "AuthorizerReturnsWrongInitialState" or message =~ "initial_state"

          assert message =~ "map" or message =~ "state",
                 "message should describe expected return: #{message}"
      end
    end

    test "protected_fields defaults to an empty list" do
      assert Ash.Authorizer.protected_fields(AuthorizerWithoutProtectedFields, AuthorizerResource) ==
               []
    end

    test "protected_fields validates a list of atoms" do
      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Authorizer.protected_fields(AuthorizerReturnsWrongProtectedFields, AuthorizerResource)
      end
    end

    test "resource info aggregates protected fields from authorizers" do
      assert Ash.Resource.Info.protected_fields(ProtectedFieldsResource) == [:name, :email]
    end

    defmodule AuthorizerReturnsWrongStrictCheck do
      use Ash.Authorizer

      def initial_state(_actor, _resource, _action, _domain), do: %{}
      def strict_check_context(_state), do: [:query]
      def strict_check(_state, _context), do: :wrong
      def check_context(_state), do: [:query]
      def check(_state, _context), do: :authorized
    end

    test "raises InvalidReturnType when strict_check returns wrong shape" do
      assert_raise Ash.Error.Framework.InvalidReturnType, fn ->
        Ash.Authorizer.strict_check(
          AuthorizerReturnsWrongStrictCheck,
          %{},
          %{}
        )
      end
    end

    test "strict_check error message includes callback name and expected shapes" do
      try do
        Ash.Authorizer.strict_check(
          AuthorizerReturnsWrongStrictCheck,
          %{},
          %{}
        )
      rescue
        e in [Ash.Error.Framework.InvalidReturnType] ->
          message = Exception.message(e)
          assert message =~ "strict_check/2", "message should include callback name: #{message}"
          assert message =~ "AuthorizerReturnsWrongStrictCheck" or message =~ "strict_check"

          assert message =~ "authorized" or message =~ "continue" or message =~ "expects",
                 "message should describe expected return shapes: #{message}"
      end
    end
  end

  # A data layer may return `{:error, :no_rollback, error}` from any query callback (AshPostgres
  # does when an `error/2` expression raises, e.g. with `authorize_with: :error`). Counts,
  # aggregates and lateral join queries must turn it into an error instead of raising.
  describe "Ash.DataLayer {:error, :no_rollback, error}" do
    defmodule NoRollbackEts do
      @moduledoc false
      @behaviour Ash.DataLayer

      @overridden [
        :can?,
        :run_aggregate_query,
        :run_aggregate_query_with_lateral_join,
        :run_query_with_lateral_join
      ]

      # Run inline, so counts aren't raced against the rows query in a task.
      def can?(_resource, :async_engine), do: false
      def can?(resource, feature), do: Ash.DataLayer.Ets.can?(resource, feature)

      # Each test names the callback that fails; the others run against ETS as usual.
      def run_aggregate_query(query, aggregates, resource) do
        maybe_fail(:run_aggregate_query, [query, aggregates, resource])
      end

      def run_aggregate_query_with_lateral_join(query, aggregates, root, resource, path) do
        maybe_fail(:run_aggregate_query_with_lateral_join, [
          query,
          aggregates,
          root,
          resource,
          path
        ])
      end

      def run_query_with_lateral_join(query, root, resource, path) do
        maybe_fail(:run_query_with_lateral_join, [query, root, resource, path])
      end

      defp maybe_fail(callback, args) do
        if Process.get(:no_rollback_callback) == callback do
          {:error, :no_rollback, Ash.Error.Forbidden.exception([])}
        else
          apply(Ash.DataLayer.Ets, callback, args)
        end
      end

      for {name, arity} <- Ash.DataLayer.Ets.__info__(:functions),
          name not in [:spark_dsl_config, :explain | @overridden] and
            not String.starts_with?(Atom.to_string(name), "__") do
        args = Macro.generate_arguments(arity, __MODULE__)
        defdelegate unquote(name)(unquote_splicing(args)), to: Ash.DataLayer.Ets
      end
    end

    defmodule NoRollbackPost do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: NoRollbackEts

      actions do
        defaults [:read, create: [:author_id]]

        read :paginated do
          pagination offset?: true, countable: true, required?: false
        end
      end

      attributes do
        uuid_primary_key :id
      end

      relationships do
        belongs_to :author, Ash.BehaviourReturnValidationTest.NoRollbackAuthor, public?: true
      end
    end

    defmodule NoRollbackAuthor do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      actions do
        defaults [:read, :create]
      end

      attributes do
        uuid_primary_key :id
      end

      relationships do
        has_many :posts, NoRollbackPost, public?: true
      end
    end

    setup do
      authors =
        for _ <- 1..2 do
          author = Ash.create!(NoRollbackAuthor, %{})
          Ash.create!(NoRollbackPost, %{author_id: author.id})
          author
        end

      %{authors: authors}
    end

    test "a page count that returns :no_rollback is an error" do
      Process.put(:no_rollback_callback, :run_aggregate_query)

      assert {:error, %Ash.Error.Forbidden{}} =
               NoRollbackPost
               |> Ash.Query.for_read(:paginated)
               |> Ash.read(page: [limit: 1, count: true])
    end

    test "an aggregate query that returns :no_rollback is an error" do
      Process.put(:no_rollback_callback, :run_aggregate_query)
      assert {:error, %Ash.Error.Forbidden{}} = Ash.count(NoRollbackPost)
    end

    # A limited load for more than one record runs as a lateral join.
    test "a lateral join query that returns :no_rollback is an error", %{authors: authors} do
      Process.put(:no_rollback_callback, :run_query_with_lateral_join)

      assert {:error, %Ash.Error.Forbidden{}} =
               Ash.load(authors, posts: Ash.Query.limit(NoRollbackPost, 1))
    end

    # Relationship page counts come from aggregates on the parent, so no load reaches this
    # callback; call the wrapper directly.
    test "a lateral join aggregate query that returns :no_rollback is an error" do
      Process.put(:no_rollback_callback, :run_aggregate_query_with_lateral_join)

      assert {:error, %Ash.Error.Forbidden{}} =
               Ash.DataLayer.run_aggregate_query_with_lateral_join(
                 nil,
                 [],
                 [],
                 NoRollbackPost,
                 []
               )
    end
  end
end
