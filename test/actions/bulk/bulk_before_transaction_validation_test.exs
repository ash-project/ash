# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.BulkBeforeTransactionValidationTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule RecordingValidation do
    @moduledoc false
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, opts, _context) do
      send(self(), {:ran, opts[:tag]})

      if Ash.Changeset.get_attribute(changeset, :name) == "bad" do
        {:error, field: :name, message: "name is bad"}
      else
        :ok
      end
    end
  end

  defmodule BatchRecordingValidation do
    @moduledoc false
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, opts, _context) do
      send(self(), {:ran, {opts[:tag], :validate}})

      if Ash.Changeset.get_attribute(changeset, :name) == "bad" do
        {:error, field: :name, message: "name is bad"}
      else
        :ok
      end
    end

    @impl true
    def batch_validate(changesets, opts, _context) do
      send(self(), {:ran, {opts[:tag], :batch_validate, length(changesets)}})

      Enum.map(changesets, fn changeset ->
        if Ash.Changeset.get_attribute(changeset, :name) == "bad" do
          Ash.Changeset.add_error(changeset, field: :name, message: "name is bad")
        else
          changeset
        end
      end)
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

      create :batch_create do
        accept [:name]

        validate {BatchRecordingValidation, tag: :batch_validation}, before_transaction?: true
      end

      create :plain_create do
        accept [:name]
      end

      update :update do
        require_atomic? false
        accept [:name]

        change before_transaction(fn changeset, _context ->
                 send(self(), {:ran, :user_hook})
                 changeset
               end)

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      update :update_just_validation do
        require_atomic? false
        accept [:name]

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      destroy :destroy do
        require_atomic? false

        change before_transaction(fn changeset, _context ->
                 send(self(), {:ran, :user_hook})
                 changeset
               end)

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      destroy :destroy_just_validation do
        require_atomic? false

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      destroy :soft_destroy do
        require_atomic? false
        soft? true
        change set_attribute(:archived?, true)

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :archived?, :boolean, public?: true, default: false
    end
  end

  defmodule StrictPost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read]

      create :create do
        accept [:name, :title]

        validate {RecordingValidation, tag: :validation}, before_transaction?: true
      end

      create :create_only_when_valid do
        accept [:name, :title]

        validate {RecordingValidation, tag: :validation},
          before_transaction?: true,
          only_when_valid?: true
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :title, :string, public?: true, allow_nil?: false
    end
  end

  defp create_post!(name) do
    Post
    |> Ash.Changeset.for_create(:plain_create, %{name: name})
    |> Ash.create!()
  end

  describe "bulk create" do
    test "a passing before_transaction? validation allows the create" do
      result =
        Ash.bulk_create([%{name: "ok"}], Post, :create,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      assert [%{name: "ok"}] = result.records
      assert_received {:ran, :validation}
    end

    test "the validation runs before other before_transaction hooks" do
      result = Ash.bulk_create([%{name: "ok"}], Post, :create, return_errors?: true)

      assert result.status == :success
      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :validation
      assert second == :user_hook
    end

    test "a failing validation surfaces as an invalid error and short-circuits later hooks" do
      result =
        Ash.bulk_create([%{name: "bad"}], Post, :create,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      assert [
               %Ash.Error.Invalid{
                 errors: [
                   %Ash.Error.Changes.InvalidAttribute{field: :name, message: "name is bad"}
                 ]
               }
             ] = result.errors

      assert_received {:ran, :validation}
      refute_received {:ran, :user_hook}
    end

    test "a mixed batch creates the valid records and errors on the invalid ones" do
      result =
        Ash.bulk_create([%{name: "ok"}, %{name: "bad"}], Post, :create,
          return_records?: true,
          return_errors?: true,
          stop_on_error?: false
        )

      assert result.status == :partial_success
      assert [%{name: "ok"}] = result.records
      assert result.error_count == 1
    end

    test "the validation is not run at build time when the changeset becomes invalid before hooks" do
      # :title is allow_nil?: false, so the changeset becomes invalid when required
      # values are checked, which happens after validations are dispatched but before
      # before_transaction hooks run. The validation must not run for such changesets.
      result =
        Ash.bulk_create([%{name: "ok"}], StrictPost, :create,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :error
      refute_received {:ran, :validation}
    end

    test "only_when_valid? validations are also skipped for changesets that become invalid" do
      result =
        Ash.bulk_create([%{name: "ok"}], StrictPost, :create_only_when_valid,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :error
      refute_received {:ran, :validation}
    end

    test "batch-capable validations run per changeset in the hook phase" do
      result =
        Ash.bulk_create([%{name: "ok"}, %{name: "also ok"}], Post, :batch_create,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      # at hook time the validation runs once per changeset, not once per batch
      assert_received {:ran, {:batch_validation, :batch_validate, 1}}
      assert_received {:ran, {:batch_validation, :batch_validate, 1}}
      refute_received {:ran, {:batch_validation, :batch_validate, 2}}
    end

    test "a failing batch-capable validation surfaces errors" do
      result =
        Ash.bulk_create([%{name: "bad"}], Post, :batch_create,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1
    end
  end

  describe "bulk update" do
    test "the validation runs before other before_transaction hooks" do
      post = create_post!("ok")

      result =
        Ash.bulk_update([post], :update, %{name: "new"},
          strategy: [:stream],
          resource: Post,
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      assert [%{name: "new"}] = result.records
      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :validation
      assert second == :user_hook
    end

    test "a failing validation surfaces the error and short-circuits later hooks" do
      post = create_post!("ok")

      result =
        Ash.bulk_update([post], :update, %{name: "bad"},
          strategy: [:stream],
          resource: Post,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      assert [
               %Ash.Error.Invalid{
                 errors: [
                   %Ash.Error.Changes.InvalidAttribute{field: :name, message: "name is bad"}
                 ]
               }
             ] = result.errors

      assert_received {:ran, :validation}
      refute_received {:ran, :user_hook}
    end

    test "an atomic-only strategy reports that the validation cannot be run atomically" do
      create_post!("ok")

      result =
        Ash.bulk_update(Post, :update_just_validation, %{name: "new"},
          strategy: [:atomic],
          return_errors?: true
        )

      assert result.status == :error
      assert [error] = result.errors
      assert Exception.message(error) =~ "cannot be run atomically"
      refute_received {:ran, :validation}
    end

    test "an atomic strategy falls back to streaming when allowed" do
      create_post!("ok")

      result =
        Ash.bulk_update(Post, :update_just_validation, %{name: "new"},
          strategy: [:atomic, :stream],
          return_records?: true,
          return_errors?: true
        )

      assert result.status == :success
      assert [%{name: "new"}] = result.records
      assert_received {:ran, :validation}
    end
  end

  describe "bulk destroy" do
    test "the validation runs before other before_transaction hooks" do
      post = create_post!("ok")

      result =
        Ash.bulk_destroy([post], :destroy, %{},
          strategy: [:stream],
          resource: Post,
          return_errors?: true
        )

      assert result.status == :success
      assert_received {:ran, first}
      assert_received {:ran, second}
      assert first == :validation
      assert second == :user_hook
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Post, post.id)
    end

    test "a failing validation surfaces the error and prevents the destroy" do
      post = create_post!("bad")

      result =
        Ash.bulk_destroy([post], :destroy, %{},
          strategy: [:stream],
          resource: Post,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1

      assert [
               %Ash.Error.Invalid{
                 errors: [
                   %Ash.Error.Changes.InvalidAttribute{field: :name, message: "name is bad"}
                 ]
               }
             ] = result.errors

      assert_received {:ran, :validation}
      refute_received {:ran, :user_hook}
      assert %{name: "bad"} = Ash.get!(Post, post.id)
    end

    test "an atomic strategy falls back to streaming when allowed" do
      post = create_post!("ok")

      result =
        Ash.bulk_destroy(Post, :destroy_just_validation, %{},
          strategy: [:atomic, :stream],
          return_errors?: true
        )

      assert result.status == :success
      assert_received {:ran, :validation}
      assert {:error, %Ash.Error.Invalid{}} = Ash.get(Post, post.id)
    end

    test "a soft destroy runs the validation and archives the record" do
      post = create_post!("ok")

      result =
        Ash.bulk_destroy([post], :soft_destroy, %{},
          strategy: [:stream],
          resource: Post,
          return_errors?: true
        )

      assert result.status == :success
      assert_received {:ran, :validation}
      assert %{name: "ok", archived?: true} = Ash.get!(Post, post.id)
    end

    test "a failing validation prevents a soft destroy" do
      post = create_post!("bad")

      result =
        Ash.bulk_destroy([post], :soft_destroy, %{},
          strategy: [:stream],
          resource: Post,
          return_errors?: true
        )

      assert result.status == :error
      assert result.error_count == 1
      assert_received {:ran, :validation}
      assert %{name: "bad", archived?: false} = Ash.get!(Post, post.id)
    end
  end
end
