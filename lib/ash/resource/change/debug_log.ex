defmodule Ash.Resource.Change.DebugLog do
  @moduledoc false
  use Ash.Resource.Change
  require Logger

  @doc false
  @impl true
  @spec change(Ash.Changeset.t(), keyword, Ash.Resource.Change.context()) :: Ash.Changeset.t()
  def change(changeset, opts, _) do
    label = opts[:label]
    debug(changeset.params, "input", label, changeset)
    debug(changeset.attributes, "casted attributes", label, changeset)
    debug(changeset.arguments, "casted arguments", label, changeset)

    changeset
    |> Ash.Changeset.after_transaction(fn changeset, result ->
      debug(changeset, "final changeset", label, changeset)
      debug(result, "action result", label, changeset)
      result
    end)
    |> Ash.Changeset.around_action(fn changeset, callback ->
      try do
        callback.(changeset)
      rescue
        e ->
          debug_exception(
            Exception.format(:error, e, __STACKTRACE__),
            "action raised error",
            label,
            changeset
          )

          reraise e, __STACKTRACE__
      end
    end)
  end

  @impl true
  def atomic(changeset, opts, _context) do
    label = opts[:label]
    debug(changeset, "building atomic", label, changeset)
    :ok
  end

  defp debug(stuff, our_label, nil, changeset) do
    Logger.debug(
      "#{our_label} - #{inspect(changeset.resource)}.#{changeset.action.name}: #{inspect(stuff)}"
    )
  end

  defp debug(stuff, our_label, their_label, changeset) do
    Logger.debug(
      "#{their_label} - #{our_label} - #{inspect(changeset.resource)}.#{changeset.action.name}: #{inspect(stuff)}"
    )
  end

  defp debug_exception(stuff, our_label, nil, changeset) do
    Logger.debug(
      "#{our_label} - #{inspect(changeset.resource)}.#{changeset.action.name}: #{stuff}"
    )
  end

  defp debug_exception(stuff, our_label, their_label, changeset) do
    Logger.debug(
      "#{their_label} - #{our_label} - #{inspect(changeset.resource)}.#{changeset.action.name}: #{stuff}"
    )
  end
end
