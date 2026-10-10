# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyEmitSignal do
  @moduledoc """
  Verifies `emit_signal` changes and preparations: that the signal exists, that every option
  refers to fields that exist, and that every required field of the signal is set.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.{Entity, Verifier}
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Ash.Signals.Emitter.usages()
    |> Enum.each(&verify_usage(&1, module, dsl_state))

    :ok
  end

  defp verify_usage({kind, action, entity, opts}, module, dsl_state) do
    error = &raise_error(module, entity, kind, &1)
    signal_module = opts[:signal_module]

    signal =
      if Code.ensure_loaded?(signal_module) && Spark.Dsl.is?(signal_module, Ash.Signals) do
        Ash.Signals.Info.signal(signal_module, opts[:signal]) ||
          error.("`#{inspect(signal_module)}` has no signal named `#{inspect(opts[:signal])}`.")
      else
        error.("`#{inspect(signal_module)}` is not an `Ash.Signals` module.")
      end

    if opts[:phase] != signal.phase do
      error.(
        "Signal `#{inspect(signal.name)}` is emitted during `#{inspect(signal.phase)}`, so the phase given to `emit_signal` must be `#{inspect(signal.phase)}`, got: #{inspect(opts[:phase])}"
      )
    end

    fields = Ash.Resource.SignalListener.normalize_mapping(opts[:fields] || [])
    values = opts[:values] || []
    previous = Ash.Signals.Emitter.previous_fields(opts)
    signal_fields = Enum.map(signal.arguments, & &1.name)

    for {option, mapping} <- [fields: fields, values: values],
        {name, _} <- mapping,
        name not in signal_fields do
      error.(
        "`#{option}` sets `#{inspect(name)}`, but signal `#{inspect(signal.name)}` has no field with that name."
      )
    end

    if kind == :change do
      verify_change(error, action, entity, fields, previous, dsl_state)
    else
      if previous != [] do
        error.("`previous/1` is only for update and destroy actions.")
      end
    end

    explicit = Keyword.keys(fields) ++ Keyword.keys(values)

    for %{name: name, allow_nil?: false, default: nil} <- signal.arguments,
        name not in explicit,
        !default_source?(kind, action, name, dsl_state) do
      source =
        if kind == :change,
          do: "an attribute, calculation or aggregate of the resource",
          else: "an argument of the action"

      error.(
        "Signal `#{inspect(signal.name)}` requires `#{inspect(name)}`, which is not #{source}. Set it with `fields` or `values`."
      )
    end
  end

  defp verify_change(error, action, entity, fields, previous, dsl_state) do
    for field <- Keyword.values(fields) ++ previous, !resource_field?(field, dsl_state) do
      error.("`#{inspect(field)}` is not an attribute, calculation or aggregate of the resource.")
    end

    if previous != [] do
      types = if action, do: [action.type], else: entity.on

      if :create in types do
        error.("`previous/1` is only for update and destroy actions.")
      end
    end
  end

  defp default_source?(:change, _action, name, dsl_state), do: resource_field?(name, dsl_state)

  defp default_source?(:prepare, nil, _name, _dsl_state), do: true

  defp default_source?(:prepare, action, name, _dsl_state),
    do: Enum.any?(action.arguments, &(&1.name == name))

  defp resource_field?(name, dsl_state) do
    !!(Ash.Resource.Info.attribute(dsl_state, name) ||
         Ash.Resource.Info.calculation(dsl_state, name) ||
         Ash.Resource.Info.aggregate(dsl_state, name))
  end

  defp raise_error(module, entity, kind, message) do
    raise DslError,
      module: module,
      path: [if(kind == :change, do: :change, else: :prepare), :emit_signal],
      location: Entity.anno(entity),
      message: message
  end
end
