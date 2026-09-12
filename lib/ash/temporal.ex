# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Temporal do
  @moduledoc """
  The `as_of` algebra: the instant a read sees, and the period a write establishes.

    * `resolve_as_of/1` — the instant a read sees
    * `write_instant/2` — the instant a write supersedes at
    * `write_period/2` — the period a write establishes, `[instant, ∞)`
    * `now_for/1` — the current instant in an extent

  Each answers in the inner type of the resource's period, so `:now` is a `Date` on a
  `:date` extent and a second-precision `DateTime` on a `:datetime` one.

  A temporal data layer passes `:now` for a write that names no `as_of`.
  """

  @typedoc "An `as_of` as a caller may give it, before it is resolved."
  @type as_of :: :now | term() | nil

  @doc """
  The instant a read "as of" `as_of` sees.

  `:now` resolves to the current instant; `nil` is unset and stays so.
  """
  @spec resolve_as_of(as_of()) :: term() | nil
  def resolve_as_of(:now), do: DateTime.utc_now()
  def resolve_as_of(other), do: other

  @doc """
  The period a write "as of" `as_of` establishes, in `resource`'s period type.

  An instant opens a period at itself: `[instant, ∞)`.
  """
  @spec write_period(Ash.Resource.t(), as_of()) :: {:ok, Ash.Range.t()} | :error
  def write_period(resource, as_of) do
    case write_instant(resource, as_of) do
      {:ok, instant} -> {:ok, %Ash.Range{lower: instant}}
      :error -> :error
    end
  end

  @doc """
  The instant a write "as of" `as_of` supersedes at, in `resource`'s inner type.
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
  The current instant in `inner_type`.

  A datetime extent answers a `DateTime`, `:naive_datetime` a `NaiveDateTime`, `:date` a
  `Date`. Any other answers `:error`.
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
