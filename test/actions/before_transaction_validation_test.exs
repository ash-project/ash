# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BeforeTransactionValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule RecordingValidation do
    @moduledoc false
    use Ash.Resource.Validation

    @impl true
    def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

    @impl true
    def validate(subject, opts, _context) do
      send(self(), {:ran, opts[:tag]})

      name =
        case subject do
          %Ash.Changeset{} = changeset -> Ash.Changeset.get_attribute(changeset, :name)
          %Ash.ActionInput{} = input -> input.arguments[:name]
          _ -> nil
        end

      cond do
        opts[:fail?] -> {:error, field: :name, message: "forced failure"}
        name == "bad" -> {:error, field: :name, message: "name is bad"}
        true -> :ok
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create do
        accept [:name]

        change before_transaction(fn changeset, _context ->
                 send(self(), {:ran, :user_hook})
                 changeset
               end)

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      update :update do
        require_atomic? false
        accept [:name]

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      update :atomic_update do
        require_atomic? true
        accept [:name]

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      read :read_with_validation do
        prepare fn query, _context ->
          Ash.Query.before_transaction(query, fn query ->
            send(self(), {:ran, :user_hook})
            query
          end)
        end

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      read :read_failing do
        validate {RecordingValidation, tag: :validation, fail?: true}, before_transaction?: true
      end

      action :generic_action, :string do
        argument :name, :string, allow_nil?: false

        validate {RecordingValidation, tag: :validation}, before_transaction?: true

        run fn _input, _context -> {:ok, "ran"} end
      end

      create :create_with_required_argument do
        accept [:name]
        argument :req, :string, allow_nil?: false

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      create :create_only_when_valid do
        accept [:name]
        argument :req, :string, allow_nil?: false

        validate {RecordingValidation, tag: :validation},
          before_transaction?: true,
          only_when_valid?: true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end
  end

  defmodule GlobalPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    validations do
      validate {RecordingValidation, tag: :global_validation},
        before_transaction?: true,
        on: [:create]
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create do
        accept [:name]
        delay_global_validations? true

        change before_transaction(fn changeset, _context ->
                 send(self(), {:ran, :user_hook})
                 changeset
               end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end
  end

  describe "create actions" do
    test "a passing before_transaction? validation allows the create" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "ok"})
        |> Ash.create!()

      assert post.name == "ok"
      assert_received {:ran, :validation}
    end

    test "a failing before_transaction? validation surfaces the error and prevents the create" do
      assert_raise Ash.Error.Invalid, ~r/name is bad/, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{name: "bad"})
        |> Ash.create!()
      end
    end

    test "the validation runs before user before_transaction hooks" do
      Post
      |> Ash.Changeset.for_create(:create, %{name: "ok"})
      |> Ash.create!()

      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :validation
      assert second == :user_hook
    end

    test "a failing validation short-circuits later before_transaction hooks" do
      assert_raise Ash.Error.Invalid, fn ->
        Post
        |> Ash.Changeset.for_create(:create, %{name: "bad"})
        |> Ash.create!()
      end

      assert_received {:ran, :validation}
      refute_received {:ran, :user_hook}
    end
  end

  describe "update actions" do
    test "a failing before_transaction? validation prevents the update" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "ok"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/name is bad/, fn ->
        post
        |> Ash.Changeset.for_update(:update, %{name: "bad"})
        |> Ash.update!()
      end
    end

    test "an atomic action with a before_transaction? validation cannot be run atomically" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "ok"})
        |> Ash.create!()

      assert_raise Ash.Error.Framework, ~r/cannot be run atomically/, fn ->
        post
        |> Ash.Changeset.for_update(:atomic_update, %{name: "ok"})
        |> Ash.update!()
      end
    end
  end

  describe "generic actions" do
    test "a passing before_transaction? validation allows the action" do
      assert {:ok, "ran"} =
               Post
               |> Ash.ActionInput.for_action(:generic_action, %{name: "ok"})
               |> Ash.run_action()

      assert_received {:ran, :validation}
    end

    test "a failing before_transaction? validation surfaces the error" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.ActionInput.for_action(:generic_action, %{name: "bad"})
               |> Ash.run_action()
    end
  end

  describe "delay_global_validations? interplay" do
    test "a global before_transaction? validation still runs in the before_transaction hook" do
      GlobalPost
      |> Ash.Changeset.for_create(:create, %{name: "ok"})
      |> Ash.create!()

      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :global_validation
      assert second == :user_hook
    end

    test "a failing global before_transaction? validation prevents the create" do
      assert_raise Ash.Error.Invalid, ~r/name is bad/, fn ->
        GlobalPost
        |> Ash.Changeset.for_create(:create, %{name: "bad"})
        |> Ash.create!()
      end

      assert_received {:ran, :global_validation}
      refute_received {:ran, :user_hook}
    end
  end

  describe "changesets that are invalid before hooks run" do
    test "before_transaction hooks are not reached, so the validation does not run" do
      # the required argument is missing, making the changeset invalid at build
      # time. Actions do not run before_transaction hooks for changesets that are
      # already invalid, so the validation is never invoked and the original
      # error surfaces.
      result =
        Post
        |> Ash.Changeset.for_create(:create_with_required_argument, %{name: "ok"})
        |> Ash.create()

      assert {:error, %Ash.Error.Invalid{}} = result
      refute_received {:ran, :validation}
    end

    test "only_when_valid? validations are also skipped for invalid changesets" do
      result =
        Post
        |> Ash.Changeset.for_create(:create_only_when_valid, %{name: "ok"})
        |> Ash.create()

      assert {:error, %Ash.Error.Invalid{}} = result
      refute_received {:ran, :validation}
    end

    test "only_when_valid? validations run at hook time on valid changesets" do
      Post
      |> Ash.Changeset.for_create(:create_only_when_valid, %{name: "ok", req: "given"})
      |> Ash.create!()

      assert_received {:ran, :validation}
    end
  end

  describe "read actions" do
    test "the validation runs before user before_transaction hooks on reads" do
      Post
      |> Ash.Query.for_read(:read_with_validation)
      |> Ash.read!()

      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :validation
      assert second == :user_hook
    end

    test "a failing before_transaction? validation surfaces the error on reads" do
      assert {:error, %Ash.Error.Invalid{}} =
               Post
               |> Ash.Query.for_read(:read_failing)
               |> Ash.read()
    end
  end
end
