# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.LocalNow do
  @moduledoc """
  Returns the current local time in the given time zone as a `NaiveDateTime`.

  Pass an IANA time zone name, for example `local_now("Asia/Hong_Kong")`.
  In-memory evaluation uses Elixir's configured time zone database.
  Temporal queries use their `as_of` instant.
  """
  use Ash.Query.Function, name: :local_now, eager_evaluate?: false

  def args, do: [[:string]]

  def returns, do: [:naive_datetime]

  def evaluate(%{arguments: [time_zone]} = function) do
    to_local(utc_now(function), time_zone)
  end

  @doc false
  def to_local(datetime, time_zone) do
    case DateTime.shift_zone(datetime, time_zone) do
      {:ok, shifted} ->
        {:known, DateTime.to_naive(shifted)}

      {:error, reason} ->
        {:error, "Could not convert to time zone #{inspect(time_zone)} (#{reason})"}
    end
  end

  @doc false
  def utc_now(%{extra: %{as_of: %DateTime{} = as_of}}) do
    DateTime.shift_zone!(as_of, "Etc/UTC")
  end

  def utc_now(_), do: DateTime.utc_now()

  @doc false
  def put_as_of(function, %DateTime{} = as_of) do
    %{function | extra: Map.put(function.extra, :as_of, as_of)}
  end

  def put_as_of(function, nil), do: function
end
