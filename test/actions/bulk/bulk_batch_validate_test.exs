# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkBatchValidateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule BatchTitleValidation do
    @moduledoc "Validates the entire batch at once, rejecting titles containing 'invalid'"
    use Ash.Resource.Validation

    @impl true
    def batch_validate(changesets, _opts, _context) do
      send(self(), {:batch_validate_called, length(Enum.to_list(changesets))})

      Enum.map(changesets, fn changeset ->
        title = Ash.Changeset.get_attribute(changeset, :title)

        if title && String.contains?(title, "invalid") do
          Ash.Changeset.add_error(changeset, field: :title, message: "title is invalid")
        else
          changeset
        end
      end)
    end

    @impl true
    def validate(changeset, _opts, _context) do
      title = Ash.Changeset.get_attribute(changeset, :title)

      if title && String.contains?(title, "invalid") do
        {:error, field: :title, message: "title is invalid (per-changeset)"}
      else
        :ok
      end
    end
  end

  defmodule BatchOnlyValidation do
    @moduledoc "Only defines batch_validate, no per-changeset validate"
    use Ash.Resource.Validation

    @impl true
    def batch_validate(changesets, _opts, _context) do
      send(self(), {:batch_only_validate_called, length(Enum.to_list(changesets))})

      Enum.map(changesets, fn changeset ->
        title = Ash.Changeset.get_attribute(changeset, :title)

        if title && String.contains?(title, "forbidden") do
          Ash.Changeset.add_error(changeset, field: :title, message: "title is forbidden")
        else
          changeset
        end
      end)
    end
  end

  defmodule NoBatchCallbacksValidation do
    @moduledoc "Has batch_validate but batch_callbacks? returns false, forcing per-changeset fallback"
    use Ash.Resource.Validation

    @impl true
    def batch_callbacks?(_, _, _), do: false

    @impl true
    def batch_validate(changesets, _opts, _context) do
      send(self(), {:no_batch_callbacks_batch_validate_called, length(Enum.to_list(changesets))})
      Enum.to_list(changesets)
    end

    @impl true
    def validate(changeset, _opts, _context) do
      send(self(), :no_batch_callbacks_validate_called)
      title = Ash.Changeset.get_attribute(changeset, :title)

      if title && String.contains?(title, "reject") do
        {:error, field: :title, message: "title rejected"}
      else
        :ok
      end
    end
  end

  defmodule TitleContainsValidation do
    @moduledoc "Where-condition validation: passes if title contains a given substring"
    use Ash.Resource.Validation

    @impl true
    def validate(changeset, opts, _context) do
      title = Ash.Changeset.get_attribute(changeset, :title)

      if title && String.contains?(title, opts[:substring] || "") do
        :ok
      else
        {:error, field: :title, message: "title must contain '#{opts[:substring]}'"}
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :create_with_batch_validation do
        validate BatchTitleValidation
      end

      create :create_with_batch_only_validation do
        validate BatchOnlyValidation
      end

      create :create_with_no_batch_callbacks do
        validate NoBatchCallbacksValidation
      end

      create :create_with_where_batch_validation do
        validate BatchTitleValidation,
          where: [{TitleContainsValidation, substring: "check"}]
      end

      create :create_with_only_when_valid_batch_validation do
        validate fn changeset, _context ->
          if Ash.Changeset.get_attribute(changeset, :title) == "force_error" do
            {:error, field: :title, message: "forced error"}
          else
            :ok
          end
        end

        validate BatchTitleValidation, only_when_valid?: true
      end

      update :update_with_batch_validation do
        require_atomic? false
        validate BatchTitleValidation
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public? true
        allow_nil? false
      end
    end
  end

  describe "batch_validate in bulk create" do
    test "batch_validate receives all changesets at once" do
      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create!(
                 [%{title: "one"}, %{title: "two"}, %{title: "three"}],
                 Post,
                 :create_with_batch_validation,
                 return_records?: true,
                 authorize?: false
               )

      assert length(records) == 3
      assert_received {:batch_validate_called, 3}
    end

    test "batch_validate can add errors to specific changesets" do
      result =
        Ash.bulk_create(
          [%{title: "good"}, %{title: "invalid_title"}, %{title: "also_good"}],
          Post,
          :create_with_batch_validation,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert result.status in [:partial_success, :error]
      assert result.error_count > 0
      assert_received {:batch_validate_called, _}
    end

    test "batch_callbacks? returning false causes fallback to per-changeset validate" do
      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create!(
                 [%{title: "ok1"}, %{title: "ok2"}],
                 Post,
                 :create_with_no_batch_callbacks,
                 return_records?: true,
                 authorize?: false
               )

      assert length(records) == 2
      refute_received {:no_batch_callbacks_batch_validate_called, _}
      assert_received :no_batch_callbacks_validate_called
    end

    test "where conditions filter which changesets reach batch_validate" do
      assert %Ash.BulkResult{status: :success, records: records} =
               Ash.bulk_create!(
                 [%{title: "check_this"}, %{title: "skip_this"}, %{title: "check_too"}],
                 Post,
                 :create_with_where_batch_validation,
                 return_records?: true,
                 authorize?: false
               )

      assert length(records) == 3
      assert_received {:batch_validate_called, 2}
    end

    test "only_when_valid? skips batch_validate for invalid changesets" do
      result =
        Ash.bulk_create(
          [%{title: "good"}, %{title: "force_error"}, %{title: "fine"}],
          Post,
          :create_with_only_when_valid_batch_validation,
          return_records?: true,
          return_errors?: true,
          authorize?: false
        )

      assert_received {:batch_validate_called, 2}
      assert result.error_count > 0
    end
  end

  describe "batch_validate in bulk update" do
    test "batch_validate receives all changesets during bulk update" do
      %Ash.BulkResult{records: records} =
        Ash.bulk_create!(
          [%{title: "alpha"}, %{title: "beta"}, %{title: "gamma"}],
          Post,
          :create,
          return_records?: true,
          authorize?: false
        )

      assert length(records) == 3

      assert %Ash.BulkResult{status: :success} =
               Ash.bulk_update!(records, :update_with_batch_validation, %{title: "updated"},
                 return_records?: true,
                 authorize?: false,
                 strategy: :stream
               )

      assert_received {:batch_validate_called, _}
    end

    test "batch_validate can reject records during bulk update" do
      %Ash.BulkResult{records: records} =
        Ash.bulk_create!(
          [%{title: "alpha"}, %{title: "beta"}],
          Post,
          :create,
          return_records?: true,
          authorize?: false
        )

      result =
        Ash.bulk_update(records, :update_with_batch_validation, %{title: "invalid_update"},
          return_records?: true,
          return_errors?: true,
          authorize?: false,
          strategy: :stream
        )

      assert length(result.errors) > 0
      assert_received {:batch_validate_called, _}
    end
  end
end
