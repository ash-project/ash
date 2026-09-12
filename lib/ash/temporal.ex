# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Temporal do
  @moduledoc """
  Resolves `as_of` into the point of time or period that the data layer stores.

  On a [temporal resource](/documentation/topics/advanced/temporal-resources.md) every
  version of a record is valid for a period. You can provide a `DateTime`, a `Date`, or
  `:now` for resolution by the data layer.

  ```elixir
  # a read resolves as_of a point in time
  Ash.Temporal.resolve_as_of(query.as_of)

  # a write resolves a period beginning at the point,
  # and extending forever unless a later write closes it
  {:ok, instant} = Ash.Temporal.write_instant(resource, :now)
  {:ok, period} = Ash.Temporal.write_period(resource, :now)
  ```

  The write functions return a value of the type the resource builds its periods from. On a
  `:date` resource `:now` gives you a `Date`. On a `:datetime` one it gives you a `DateTime`
  according to its constraints. Not every type has a current value. A resource that numbers
  its versions from one has no `:now` to give and the write functions return `:error`.

  A write that provides no `as_of` takes effect now. Data layers provide `:now` for this.
  """

  @typedoc "An `as_of` as a caller may give it, before it is resolved."
  @type as_of :: :now | term() | nil

  @doc """
  Resolves an `as_of` to a point in time.

  `:now` resolves to the current time. Anything else is returned unchanged. `nil` means no
  particular time was provided.
  """
  @spec resolve_as_of(as_of()) :: term() | nil
  def resolve_as_of(:now), do: DateTime.utc_now()
  def resolve_as_of(other), do: other

  @doc """
  Resolves the period a write is valid for.

  The value comes back in the type the resource builds its periods from. It begins where
  the write takes effect and extends forever unless a later write closes it.
  """
  @spec write_period(Ash.Resource.t(), as_of()) :: {:ok, Ash.Range.t()} | :error
  def write_period(resource, as_of) do
    case write_instant(resource, as_of) do
      {:ok, instant} -> {:ok, %Ash.Range{lower: instant}}
      :error -> :error
    end
  end

  @doc """
  Resolves the point where a write first takes effect.

  The value comes back in the type the resource builds its periods from. Converting applies
  whatever precision its constraints declare.
  """
  @spec write_instant(Ash.Resource.t(), as_of()) :: {:ok, term()} | :error
  def write_instant(resource, as_of) do
    inner_type = Ash.Resource.Info.temporal_inner_type(resource)

    with {:ok, raw} <- raw_instant(as_of, inner_type),
         {:ok, instant} <-
           Ash.Type.cast_input(
             inner_type,
             raw,
             Ash.Resource.Info.temporal_inner_constraints(resource) || []
           ) do
      {:ok, instant}
    else
      _ -> :error
    end
  end

  @doc """
  Returns the current time as `inner_type`.

  `:date` gives you a `Date` and `:naive_datetime` a `NaiveDateTime`. The datetime types
  give you a `DateTime`. Anything else has no current time so this returns `:error`.
  """
  @spec now_for(Ash.Type.t() | nil) :: {:ok, term()} | :error
  # Resolved through `get_type/1`: an inner type reads back as a module, and matching the
  # short names alone silently answers `:error`.
  def now_for(inner_type) do
    case Ash.Type.get_type(inner_type) do
      type when type in [Ash.Type.DateTime, Ash.Type.UtcDatetime, Ash.Type.UtcDatetimeUsec] ->
        {:ok, DateTime.utc_now()}

      Ash.Type.NaiveDatetime ->
        {:ok, NaiveDateTime.utc_now()}

      Ash.Type.Date ->
        {:ok, Date.utc_today()}

      _ ->
        :error
    end
  end

  defp raw_instant(%DateTime{} = as_of, _inner_type), do: {:ok, as_of}
  defp raw_instant(:now, inner_type), do: now_for(inner_type)
  defp raw_instant(_as_of, _inner_type), do: :error
end
