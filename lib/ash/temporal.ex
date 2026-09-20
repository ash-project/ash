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
  Ash.Temporal.resolve_read_as_of(query.as_of)

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
  Resolves the `as_of` a write takes effect at.

  `:now` resolves to the current time. A range resolves to its lower bound. Anything else is
  returned unchanged. `nil` means no particular time was provided.
  """
  @spec resolve_write_as_of(as_of()) :: term() | nil
  def resolve_write_as_of(:now), do: DateTime.utc_now()
  def resolve_write_as_of(%Ash.Range{lower: nil}), do: nil
  def resolve_write_as_of(%Ash.Range{lower: lower}), do: resolve_write_as_of(lower)
  def resolve_write_as_of(other), do: other

  @doc """
  Resolves the `as_of` a read answers at.

  `:now` resolves to the current time. `nil` means no particular time was provided. A range
  raises `Ash.Error.Query.AsOfNotAnInstant`.
  """
  @spec resolve_read_as_of(as_of()) :: term() | nil
  def resolve_read_as_of(%Ash.Range{} = as_of) do
    raise Ash.Error.Query.AsOfNotAnInstant.exception(resource: nil, as_of: as_of)
  end

  def resolve_read_as_of(:now), do: DateTime.utc_now()
  def resolve_read_as_of(other), do: other

  @doc """
  Whether a change, validation or preparation module declares itself safe to run on a
  temporal resource, for the given options.

  Every action on a temporal resource runs "as of" a point in time, so anything that
  runs as part of one must not assume it is happening now. A module declares that it
  meets that bar with the `temporal_safe?/1` callback of its behaviour
  (`c:Ash.Resource.Change.temporal_safe?/1`, `c:Ash.Resource.Validation.temporal_safe?/1`,
  `c:Ash.Resource.Preparation.temporal_safe?/1`). A module that does not define it is
  not temporal safe.
  """
  @spec temporal_safe?(module(), Keyword.t()) :: boolean()
  def temporal_safe?(module, opts) do
    Code.ensure_loaded?(module) and function_exported?(module, :temporal_safe?, 1) and
      module.temporal_safe?(opts) == true
  end

  @doc false
  # Raises `Ash.Error.Framework.NotTemporalSafe` when `module` is about to run as part of
  # an action on a temporal resource without having declared itself temporal safe.
  # `subject` is the changeset, query or action input being acted on (or a batch of
  # changesets). Called from the `Ash.Resource.Change`/`Validation`/`Preparation`
  # dispatchers, so every path that runs one of these is covered.
  @spec assert_temporal_safe!(
          :change | :validation | :preparation,
          module(),
          Keyword.t(),
          Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t() | [Ash.Changeset.t()]
        ) :: :ok
  def assert_temporal_safe!(type, module, opts, [subject | _]),
    do: assert_temporal_safe!(type, module, opts, subject)

  def assert_temporal_safe!(_type, _module, _opts, []), do: :ok

  def assert_temporal_safe!(type, module, opts, %{resource: resource} = subject) do
    if Ash.Resource.Info.temporal?(resource) and not temporal_safe?(module, opts) do
      raise Ash.Error.to_error_class(
              Ash.Error.Framework.NotTemporalSafe.exception(
                resource: resource,
                action: Map.get(subject, :action),
                module: module,
                type: type
              )
            )
    end

    :ok
  end

  @doc """
  Resolves the period a write is valid for.

  The value comes back in the type the resource builds its periods from. It begins where
  the write takes effect and extends forever unless a later write closes it.
  """
  @spec write_period(Ash.Resource.t(), as_of()) :: {:ok, Ash.Range.t()} | :error
  def write_period(resource, %Ash.Range{} = as_of) do
    with %{type: type, constraints: constraints} <- Ash.Resource.Info.temporal_period(resource),
         {:ok, bounded} <- resolve_bounds(as_of, Ash.Resource.Info.temporal_inner_type(resource)),
         {:ok, period} <- Ash.Type.cast_input(type, bounded, constraints) do
      {:ok, period}
    else
      _ -> :error
    end
  end

  def write_period(resource, as_of) do
    case write_instant(resource, as_of) do
      {:ok, instant} -> {:ok, %Ash.Range{lower: instant}}
      :error -> :error
    end
  end

  # A bound reads `:now` off the same clock a bare `:now` does, so the two spellings agree.
  defp resolve_bounds(%Ash.Range{} = as_of, inner_type) do
    with {:ok, lower} <- resolve_bound(as_of.lower, inner_type),
         {:ok, upper} <- resolve_bound(as_of.upper, inner_type) do
      {:ok, %{as_of | lower: lower, upper: upper}}
    end
  end

  defp resolve_bound(:now, inner_type), do: now_for(inner_type)
  defp resolve_bound(bound, _inner_type), do: {:ok, bound}

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
  defp raw_instant(%Ash.Range{lower: nil}, _inner_type), do: :error
  defp raw_instant(%Ash.Range{lower: lower}, inner_type), do: resolve_bound(lower, inner_type)
  defp raw_instant(_as_of, _inner_type), do: :error
end
