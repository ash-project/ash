# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifyEmitSignal do
  @moduledoc """
  Verifies the `emit` declarations of actions: that the signal exists and is emitted during its
  phase, that every option refers to fields that exist, and that every required field of the
  signal is set.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.{Entity, Verifier}
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)

    for action <- Verifier.get_entities(dsl_state, [:actions]),
        emit <- Map.get(action, :emits, []) do
      verify_emit(emit, action, module, dsl_state)
    end

    :ok
  end

  defp verify_emit(emit, action, module, dsl_state) do
    context = {module, action, emit}
    signal_module = emit.signal_module

    signal =
      if Code.ensure_loaded?(signal_module) && Spark.Dsl.is?(signal_module, Ash.Signals) do
        Ash.Signals.Info.signal(signal_module, emit.signal) ||
          raise_error(
            context,
            "`#{inspect(signal_module)}` has no signal named `#{inspect(emit.signal)}`."
          )
      else
        raise_error(context, "`#{inspect(signal_module)}` is not an `Ash.Signals` module.")
      end

    if emit.phase != signal.phase do
      raise_error(
        context,
        "Signal `#{inspect(signal.name)}` is emitted during `#{inspect(signal.phase)}`, so `emit` must be given `phase: #{inspect(signal.phase)}`, got: #{inspect(emit.phase)}"
      )
    end

    if emit.dynamic? do
      for {option, value} <- [fields: emit.fields, values: emit.values, where: emit.where],
          value != [] do
        raise_error(
          context,
          "`#{option}` can't be used with `dynamic?: true`, because the signal is built by your own code."
        )
      end
    else
      verify_payload(emit, action, signal, context, dsl_state)
    end
  end

  defp verify_payload(emit, action, signal, context, dsl_state) do
    previous = Ash.Signals.Emitter.previous_fields(values: emit.values)
    signal_fields = Enum.map(signal.arguments, & &1.name)

    for {option, mapping} <- [fields: emit.fields, values: emit.values],
        {name, _} <- mapping,
        name not in signal_fields do
      raise_error(
        context,
        "`#{option}` sets `#{inspect(name)}`, but signal `#{inspect(signal.name)}` has no field with that name."
      )
    end

    if action.type == :action do
      if previous != [] do
        raise_error(context, "`previous/1` is only for update and destroy actions.")
      end
    else
      for {_name, field} <- emit.fields,
          !Ash.Resource.Info.attribute(dsl_state, field) do
        raise_error(
          context,
          "`#{inspect(field)}` is not an attribute of the resource. To build the signal from anything else, emit it from your own code, with `dynamic?: true`."
        )
      end

      for field <- previous, !Ash.Resource.Info.attribute(dsl_state, field) do
        raise_error(context, "`previous(#{inspect(field)})` is not an attribute of the resource.")
      end

      if previous != [] and action.type == :create do
        raise_error(context, "`previous/1` is only for update and destroy actions.")
      end
    end

    explicit = Keyword.keys(emit.fields) ++ Keyword.keys(emit.values)

    for %{name: name, allow_nil?: false, default: nil} <- signal.arguments,
        name not in explicit,
        !default_source?(action, name, dsl_state) do
      source =
        if action.type == :action,
          do: "an argument of the action",
          else: "an attribute of the resource"

      raise_error(
        context,
        "Signal `#{inspect(signal.name)}` requires `#{inspect(name)}`, which is not #{source}. Set it with `fields` or `values`."
      )
    end
  end

  defp default_source?(%{type: :action} = action, name, _dsl_state),
    do: Enum.any?(action.arguments, &(&1.name == name))

  defp default_source?(_action, name, dsl_state),
    do: !!Ash.Resource.Info.attribute(dsl_state, name)

  defp raise_error({module, action, emit}, message) do
    raise DslError,
      module: module,
      path: [:actions, action.type, action.name, :emit, emit.signal],
      location: Entity.anno(emit),
      message: message
  end
end
