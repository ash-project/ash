# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.EmitSignal do
  @moduledoc false
  # See `Ash.Resource.Change.Builtins.emit_signal/3`.
  use Ash.Resource.Change

  alias Ash.Signals.Emitter

  @impl true
  def init(opts), do: Emitter.init(opts)

  @impl true
  def atomic(changeset, opts, _context) do
    cond do
      # atomic actions don't run `before_batch/3`, or hooks added by `batch_change/3`
      opts[:phase] in [:before_action, :after_transaction] ->
        {:not_atomic,
         "signals emitted in `#{opts[:phase]}` cannot be emitted from atomic actions"}

      Emitter.previous_fields(opts) != [] ->
        {:not_atomic,
         "`previous/1` needs the record before the action, which atomic actions don't have"}

      true ->
        {:ok, changeset}
    end
  end

  # Only signals emitted during the action use `before_batch/3` and `after_batch/3`. Signals
  # emitted after the transaction use a hook per record, so the batch callbacks are skipped.
  @impl true
  def batch_callbacks?(_changesets_or_query, opts, _context),
    do: opts[:phase] != :after_transaction

  # Used for single actions, and for bulk actions without batch callbacks.
  @impl true
  def change(changeset, opts, _context) do
    case opts[:phase] do
      :before_action ->
        Ash.Changeset.before_action(changeset, fn changeset ->
          case emit(changeset, opts, [{changeset, nil}]) do
            :ok -> changeset
            {:error, error} -> Ash.Changeset.add_error(changeset, error)
          end
        end)

      :after_action ->
        Ash.Changeset.after_action(changeset, fn changeset, record ->
          with :ok <- emit(changeset, opts, [{changeset, record}]) do
            {:ok, record}
          end
        end)

      :after_transaction ->
        add_after_transaction_hook(changeset, opts)
    end
  end

  # Bulk creates call this whether or not batch callbacks are run.
  @impl true
  def batch_change(changesets, opts, _context) do
    if opts[:phase] == :after_transaction do
      Enum.map(changesets, &add_after_transaction_hook(&1, opts))
    else
      changesets
    end
  end

  defp add_after_transaction_hook(changeset, opts) do
    Ash.Changeset.after_transaction(changeset, fn
      changeset, {:ok, record} ->
        with :ok <- emit(changeset, opts, [{changeset, record}]) do
          {:ok, record}
        end

      _changeset, result ->
        result
    end)
  end

  @impl true
  def before_batch([changeset | _] = changesets, opts, _context) do
    if opts[:phase] == :before_action do
      case emit(changeset, opts, Enum.map(changesets, &{&1, nil})) do
        :ok -> changesets
        {:error, error} -> Enum.map(changesets, &Ash.Changeset.add_error(&1, error))
      end
    else
      changesets
    end
  end

  def before_batch([], _opts, _context), do: []

  @impl true
  def after_batch([{changeset, _} | _] = changesets_and_records, opts, _context) do
    with :after_action <- opts[:phase],
         {:error, error} <- emit(changeset, opts, changesets_and_records) do
      [{:error, error}]
    else
      _ -> Enum.map(changesets_and_records, fn {_, record} -> {:ok, record} end)
    end
  end

  def after_batch([], _opts, _context), do: []

  # `record` is nil before the action, when values are read from the changeset
  defp emit(changeset, opts, changesets_and_records) do
    payloads =
      Enum.map(changesets_and_records, fn
        {changeset, nil} ->
          get = &fetch_attribute(changeset, &1)
          Emitter.payload(opts, get, get, changeset.data)

        {changeset, record} ->
          get = &Map.fetch(record, &1)
          Emitter.payload(opts, get, get, changeset.data)
      end)

    Ash.Signals.emit_many(changeset, opts[:signal_module], opts[:signal], payloads)
  end

  defp fetch_attribute(changeset, name) do
    if Ash.Resource.Info.attribute(changeset.resource, name) do
      {:ok, Ash.Changeset.get_attribute(changeset, name)}
    else
      :error
    end
  end
end
