# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Temporal do
  @moduledoc """
  Resolves `as_of` into the point of time or period that the data layer stores.

  On a [temporal resource](/documentation/topics/advanced/temporal-resources.md) every
  version of a record is valid for a period of datetimes. You can provide a `DateTime`, or
  `:now`.

  ```elixir
  # a read resolves as_of a point in time
  Ash.Temporal.resolve_read_as_of(query.as_of)

  # a write resolves a period beginning at the point,
  # and extending forever unless a later write closes it
  {:ok, instant} = Ash.Temporal.write_instant(resource, :now)
  {:ok, period} = Ash.Temporal.write_period(resource, :now)
  ```

  The write functions return a `DateTime` cast to the precision the resource's period
  declares.

  A write that provides no `as_of` takes effect now. Ash resolves it once for the whole write,
  before the data layer is called, so a data layer receives a temporal write's `as_of` as an
  instant or a range, never `nil` or `:now`.
  """

  @temporal_safe_modules Application.compile_env(:ash, :temporal_safe_modules, [])

  @typedoc "An `as_of` as a caller may give it, before it is resolved."
  @type as_of :: :now | term() | nil

  @doc """
  Casts the `as_of` of a write to a temporal resource into the type its periods are built
  from, applying the precision that type declares.

  An instant or `:now` becomes an instant (see `write_instant/2`) and a range becomes a period
  (see `write_period/2`). Anything that can't be cast, and any `as_of` of a resource that
  isn't temporal, is returned unchanged.
  """
  @spec cast_write_as_of(Ash.Resource.t(), as_of()) :: term()
  def cast_write_as_of(_resource, nil), do: nil

  def cast_write_as_of(resource, as_of) do
    if Ash.Resource.Info.temporal?(resource) do
      result =
        case as_of do
          %Ash.Range{} -> write_period(resource, as_of)
          _ -> write_instant(resource, as_of)
        end

      case result do
        {:ok, cast} -> cast
        :error -> as_of
      end
    else
      as_of
    end
  end

  @doc """
  Checks the `as_of` of a write to a temporal resource, casting it as `cast_write_as_of/2` does.

  An instant must cast to the type the resource's periods are built from. A range must
  cast to the resource's period, and satisfy its constraints. Returns the cast `as_of`, or
  an `Ash.Error.Changes.InvalidAsOf` saying why it is refused. Any `as_of` of a resource
  that isn't temporal is returned unchanged.
  """
  @spec check_write_as_of(Ash.Resource.t(), as_of()) :: {:ok, term()} | {:error, Exception.t()}
  def check_write_as_of(_resource, nil), do: {:ok, nil}

  def check_write_as_of(resource, as_of) do
    if Ash.Resource.Info.temporal?(resource) do
      do_check_write_as_of(resource, as_of)
    else
      {:ok, as_of}
    end
  end

  defp do_check_write_as_of(resource, %Ash.Range{} = as_of) do
    %{type: type, constraints: constraints} = Ash.Resource.Info.temporal_period(resource)

    with {:ok, period} <- cast_or_refuse(resource, as_of, write_period(resource, as_of)),
         {:ok, _} <-
           refuse_unless_ok(
             resource,
             as_of,
             Ash.Type.apply_constraints(type, period, constraints)
           ) do
      {:ok, period}
    end
  end

  defp do_check_write_as_of(resource, as_of),
    do: cast_or_refuse(resource, as_of, write_instant(resource, as_of))

  defp cast_or_refuse(_resource, _as_of, {:ok, cast}), do: {:ok, cast}

  defp cast_or_refuse(resource, as_of, :error),
    do:
      {:error,
       invalid_as_of(
         resource,
         as_of,
         "an `as_of` is an instant of the resource's period, `:now`, or a range"
       )}

  defp refuse_unless_ok(_resource, _as_of, {:ok, value}), do: {:ok, value}

  defp refuse_unless_ok(resource, as_of, {:error, [{key, _} | _] = error}) when is_atom(key),
    do: {:error, invalid_as_of(resource, as_of, error[:message] || "invalid", error[:vars] || [])}

  defp refuse_unless_ok(resource, as_of, {:error, [first | _]}),
    do: refuse_unless_ok(resource, as_of, {:error, first})

  defp refuse_unless_ok(resource, as_of, {:error, message}) when is_binary(message),
    do: {:error, invalid_as_of(resource, as_of, message)}

  defp refuse_unless_ok(resource, as_of, _error),
    do: {:error, invalid_as_of(resource, as_of, "invalid")}

  defp invalid_as_of(resource, as_of, message, vars \\ []) do
    Ash.Error.Changes.InvalidAsOf.exception(
      resource: resource,
      as_of: as_of,
      message: message,
      vars: vars
    )
  end

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

  `:now` resolves to the current time, and a `DateTime` is that instant. `nil` means no
  particular time was provided. Anything else, such as a range or a `Date`, raises
  `Ash.Error.Query.AsOfNotAnInstant`.
  """
  @spec resolve_read_as_of(as_of()) :: DateTime.t() | nil
  def resolve_read_as_of(:now), do: DateTime.utc_now()
  def resolve_read_as_of(nil), do: nil
  def resolve_read_as_of(%DateTime{} = as_of), do: as_of

  def resolve_read_as_of(as_of) do
    raise Ash.Error.Query.AsOfNotAnInstant.exception(resource: nil, as_of: as_of)
  end

  @doc """
  Whether a change, validation or preparation module declares itself safe to run on a
  temporal resource, for the given options.

  Every action on a temporal resource runs "as of" a point in time, so anything that
  runs as part of one must not assume it is happening now. A module declares that it
  meets that bar with the `temporal_safe?/1` callback of its behaviour
  (`c:Ash.Resource.Change.temporal_safe?/1`, `c:Ash.Resource.Validation.temporal_safe?/1`,
  `c:Ash.Resource.Preparation.temporal_safe?/1`). A module that does not define it is
  not temporal safe.

  Modules from packages that don't declare it yet can be listed as temporal safe in
  config:

      config :ash, :temporal_safe_modules, [SomePackage.Changes.DoesThing]
  """
  @spec temporal_safe?(module(), Keyword.t()) :: boolean()
  def temporal_safe?(module, opts) do
    Enum.member?(@temporal_safe_modules, module) or
      (Code.ensure_loaded?(module) and function_exported?(module, :temporal_safe?, 1) and
         module.temporal_safe?(opts) == true)
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
          Ash.Changeset.t()
          | Ash.Query.t()
          | Ash.ActionInput.t()
          | [Ash.Changeset.t()]
          | Enumerable.t(Ash.Changeset.t())
        ) :: :ok
  def assert_temporal_safe!(type, module, opts, [subject | _]),
    do: assert_temporal_safe!(type, module, opts, subject)

  def assert_temporal_safe!(_type, _module, _opts, []), do: :ok

  def assert_temporal_safe!(_type, _module, _opts, %Stream{}), do: :ok

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

  The value comes back cast to the resource's period. It begins where the write takes effect
  and extends forever unless a later write closes it.
  """
  @spec write_period(Ash.Resource.t(), as_of()) :: {:ok, Ash.Range.t()} | :error
  def write_period(resource, %Ash.Range{} = as_of) do
    with %{type: type, constraints: constraints} <- Ash.Resource.Info.temporal_period(resource),
         bounded = resolve_bounds(as_of),
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
  defp resolve_bounds(%Ash.Range{} = as_of) do
    %{as_of | lower: resolve_bound(as_of.lower), upper: resolve_bound(as_of.upper)}
  end

  defp resolve_bound(:now), do: DateTime.utc_now()
  defp resolve_bound(bound), do: bound

  @doc """
  Resolves the point where a write first takes effect.

  The value comes back as a `DateTime`, cast to whatever precision the resource's period
  declares.
  """
  @spec write_instant(Ash.Resource.t(), as_of()) :: {:ok, DateTime.t()} | :error
  def write_instant(resource, as_of) do
    with inner_type when not is_nil(inner_type) <- Ash.Resource.Info.temporal_inner_type(resource),
         {:ok, raw} <- raw_instant(as_of),
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

  defp raw_instant(%DateTime{} = as_of), do: {:ok, as_of}
  defp raw_instant(:now), do: {:ok, DateTime.utc_now()}
  defp raw_instant(%Ash.Range{lower: nil}), do: :error
  defp raw_instant(%Ash.Range{lower: lower}), do: raw_instant(lower)
  defp raw_instant(_as_of), do: :error
end
