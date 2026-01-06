# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.BulkAfterTransaction do
  @moduledoc """
  Shared change modules, validations, and notifiers for bulk after_transaction tests.

  These modules are used by:
  - test/actions/bulk/bulk_create_after_transaction_test.exs
  - test/actions/bulk/bulk_update_after_transaction_test.exs
  - test/actions/bulk/bulk_destroy_after_transaction_test.exs
  """

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  # ============================================================================
  # Basic Change Modules
  # ============================================================================

  defmodule AfterTransactionChange do
    @moduledoc """
    Basic after_transaction hook that sends a message on success.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        send(self(), {:after_transaction_called, result.id})
        {:ok, result}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule MultipleAfterTransactionHooks do
    @moduledoc """
    Registers multiple after_transaction hooks to verify execution order.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      changeset
      |> Ash.Changeset.after_transaction(fn _changeset, {:ok, result} ->
        order = :erlang.unique_integer([:monotonic])
        send(self(), {:hook_1_executed, result.id, order})
        {:ok, result}
      end)
      |> Ash.Changeset.after_transaction(fn _changeset, {:ok, result} ->
        order = :erlang.unique_integer([:monotonic])
        send(self(), {:hook_2_executed, result.id, order})
        {:ok, result}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionHandlingErrors do
    @moduledoc """
    Handles both success and failure in after_transaction, sending messages for each.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          send(self(), {:after_transaction_success, result.id})
          {:ok, result}

        _changeset, {:error, error} ->
          send(self(), {:after_transaction_error, error})
          {:error, error}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  # ============================================================================
  # Error-Returning Change Modules
  # ============================================================================

  defmodule AfterTransactionReturnsError do
    @moduledoc """
    Hook that returns an error on success - tests error capture in results.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          send(self(), {:after_transaction_returning_error, result.id})
          {:error, "Hook intentionally returned error"}

        _changeset, {:error, error} ->
          {:error, error}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionFailsForSomeRecords do
    @moduledoc """
    Hook that fails for records with "fail" in title - tests partial_success status.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          if String.contains?(result.title || "", "fail") do
            send(self(), {:after_transaction_partial_failure, result.id})
            {:error, "Hook failed for title containing 'fail'"}
          else
            send(self(), {:after_transaction_partial_success, result.id})
            {:ok, result}
          end

        _changeset, {:error, error} ->
          {:error, error}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionRaisesException do
    @moduledoc """
    Hook that raises an exception for records with "fail" in title.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        if String.contains?(result.title || "", "fail") do
          send(self(), {:before_exception_raise, result.id})
          raise "Hook intentionally raised exception"
        else
          send(self(), {:after_transaction_success, result.id})
          {:ok, result}
        end
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionWithStopOnError do
    @moduledoc """
    Hook that returns error for records with "stop_here" in title - tests stop_on_error?.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          if String.contains?(result.title || "", "stop_here") do
            send(self(), {:hook_error_for_stop_on_error, result.id})
            {:error, "Hook error to trigger stop"}
          else
            send(self(), {:hook_success_for_stop_on_error, result.id})
            {:ok, result}
          end

        _changeset, {:error, error} ->
          {:error, error}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  # ============================================================================
  # Error-Converting Change Modules
  # ============================================================================

  defmodule AfterTransactionConvertsErrorToSuccess do
    @moduledoc """
    Hook that converts validation errors to success - returns original data as "recovered".
    Preserves author_id if the resource has that attribute.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          send(self(), {:after_transaction_success, result.id})
          {:ok, result}

        changeset, {:error, _original_error} ->
          send(self(), {:after_transaction_converted_error})

          base_record = %{
            changeset.data
            | id: Ash.UUID.generate(),
              title: "recovered_from_error"
          }

          # Preserve author_id if the resource has it (for create actions)
          record =
            if Map.has_key?(changeset.data, :author_id) do
              author_id = Ash.Changeset.get_attribute(changeset, :author_id)
              %{base_record | author_id: author_id}
            else
              base_record
            end

          {:ok, record}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterTransactionModifiesError do
    @moduledoc """
    Hook that modifies the error message - tests hook return value handling.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn
        _changeset, {:ok, result} ->
          {:ok, result}

        _changeset, {:error, _original_error} ->
          send(self(), {:after_transaction_modified_error})
          {:error, "custom error from hook"}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  defmodule AfterActionFailsWithAfterTransaction do
    @moduledoc """
    after_action fails for titles containing "fail", after_transaction converts to success.
    Tests error-to-success conversion flow.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      changeset
      |> Ash.Changeset.after_action(fn _changeset, result ->
        if String.contains?(result.title || "", "fail") do
          {:error,
           Ash.Error.Changes.InvalidAttribute.exception(
             field: :title,
             message: "title cannot contain fail"
           )}
        else
          {:ok, result}
        end
      end)
      |> Ash.Changeset.after_transaction(fn
        _changeset, {:ok, result} ->
          {:ok, result}

        changeset, {:error, _original_error} ->
          send(self(), {:after_action_failed_converted_to_success})

          {:ok,
           %{
             changeset.data
             | id: Ash.UUID.generate(),
               title: "recovered_from_after_action_failure"
           }}
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  # ============================================================================
  # Conditional After-Action Error Change Modules
  # ============================================================================

  defmodule ConditionalAfterActionErrorWithAfterTransaction do
    @moduledoc """
    after_action fails only for records with "fail" in title.
    after_transaction sends messages to verify hook execution.
    Used to test rollback behavior when first batch succeeds and second fails.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      changeset
      |> Ash.Changeset.after_action(fn _changeset, result ->
        title = result.title || ""

        if String.contains?(title, "fail") do
          send(self(), {:conditional_after_action_error, result.id})

          {:error,
           Ash.Error.Changes.InvalidAttribute.exception(
             field: :title,
             message: "conditional error for fail title"
           )}
        else
          send(self(), {:conditional_after_action_success, result.id})
          {:ok, result}
        end
      end)
      |> Ash.Changeset.after_transaction(fn _changeset, result ->
        send(self(), {:conditional_after_transaction, result})
        result
      end)
    end

    def atomic(changeset, opts, context) do
      {:ok, change(changeset, opts, context)}
    end
  end

  # ============================================================================
  # Mnesia-Specific Change Modules
  # ============================================================================

  defmodule MnesiaAfterTransactionChange do
    @moduledoc """
    after_transaction hook for Mnesia resource - tests warning when run inside transaction.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, {:ok, result} ->
        send(self(), {:mnesia_after_transaction_called, result.id})
        {:ok, result}
      end)
    end

    def atomic(changeset, _opts, _context) do
      {:ok, change(changeset, [], %{})}
    end
  end

  # ============================================================================
  # Manual Action Change Modules
  # ============================================================================

  defmodule ManualAfterTransactionChange do
    @moduledoc """
    after_transaction hook for manual actions - handles both success and error.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, result ->
        case result do
          {:ok, record} ->
            send(self(), {:manual_after_transaction_success, record.id})
            {:ok, record}

          {:error, error} ->
            send(self(), {:manual_after_transaction_error, error})
            {:error, error}
        end
      end)
    end
  end

  defmodule ManualAfterTransactionConvertsErrorChange do
    @moduledoc """
    after_transaction hook that converts manual action errors to success.
    """
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      Ash.Changeset.after_transaction(changeset, fn _changeset, result ->
        case result do
          {:ok, record} ->
            {:ok, record}

          {:error, _error} ->
            send(self(), {:manual_after_transaction_converted_error})
            {:ok, %{changeset.data | id: Ash.UUID.generate(), title: "manual_recovered"}}
        end
      end)
    end
  end
end
