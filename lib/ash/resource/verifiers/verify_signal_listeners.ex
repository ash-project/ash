# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.VerifySignalListeners do
  @moduledoc """
  Verifies the `signals_in` section: that each signal and action exists, and that the listener's
  options fit the type of its action.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.{Entity, Verifier}
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Verifier.get_entities([:signals_in])
    |> Enum.each(&verify_listener(&1, module, dsl_state))

    :ok
  end

  defp verify_listener(listener, module, dsl_state) do
    signal_module = listener.signal_module

    signal =
      if Code.ensure_loaded?(signal_module) && Spark.Dsl.is?(signal_module, Ash.Signals) do
        Ash.Signals.Info.signal(signal_module, listener.signal) ||
          raise_error(
            listener,
            module,
            "`#{inspect(signal_module)}` has no signal named `#{inspect(listener.signal)}`."
          )
      else
        raise_error(
          listener,
          module,
          "`#{inspect(signal_module)}` is not an `Ash.Signals` module."
        )
      end

    action =
      Ash.Resource.Info.action(dsl_state, listener.action) ||
        raise_error(listener, module, "No action named `#{inspect(listener.action)}`.")

    signal_fields = Enum.map(signal.arguments, & &1.name)

    case action.type do
      :action -> verify_generic(listener, module, action, signal)
      :read -> raise_error(listener, module, "Signal listeners cannot be read actions.")
      _ -> verify_bulk(listener, module, action, signal_fields, dsl_state)
    end
  end

  defp verify_generic(listener, module, action, signal) do
    for {option, value} <- [
          inputs: listener.inputs,
          read_action: listener.read_action,
          args: non_empty(listener.args),
          get_by: non_empty(listener.get_by)
        ],
        value do
      raise_error(
        listener,
        module,
        "`#{option}` is only for create, update and destroy actions, but `#{inspect(action.name)}` is a generic action."
      )
    end

    argument = Enum.find(action.arguments, &(&1.name == listener.argument))

    example =
      if listener.batch?,
        do: "{:array, #{inspect(signal.struct)}}",
        else: inspect(signal.struct)

    cond do
      !argument ->
        raise_error(
          listener,
          module,
          "Action `#{inspect(action.name)}` must accept an argument named `#{inspect(listener.argument)}` for the signal, e.g `argument #{inspect(listener.argument)}, #{example}`."
        )

      listener.batch? and !match?({:array, _}, argument.type) ->
        raise_error(
          listener,
          module,
          "With `batch?: true`, the `#{inspect(listener.argument)}` argument of `#{inspect(action.name)}` receives a list of signals, so it must be an array, e.g `argument #{inspect(listener.argument)}, #{example}`."
        )

      true ->
        :ok
    end
  end

  defp verify_bulk(listener, module, action, signal_fields, dsl_state) do
    if listener.batch? do
      raise_error(
        listener,
        module,
        "`batch?` is only for generic actions. #{String.capitalize(to_string(action.type))} actions always handle signals in bulk."
      )
    end

    if listener.argument do
      raise_error(
        listener,
        module,
        "`argument` is only for generic actions. #{String.capitalize(to_string(action.type))} actions set their inputs from the signal with `inputs`."
      )
    end

    action_inputs = Ash.Resource.Info.action_inputs(dsl_state, action.name)

    for {input, _field} <- listener.inputs || [], input not in action_inputs do
      raise_error(
        listener,
        module,
        "`#{inspect(input)}` is not an input of `#{inspect(action.name)}`."
      )
    end

    verify_fields(listener, module, :inputs, listener.inputs || [], signal_fields)

    if action.type == :create do
      for {option, value} <- [
            read_action: listener.read_action,
            args: non_empty(listener.args),
            get_by: non_empty(listener.get_by)
          ],
          value do
        raise_error(
          listener,
          module,
          "`#{option}` is only for update and destroy actions, but `#{inspect(action.name)}` is a create action."
        )
      end
    else
      verify_query(listener, module, signal_fields, dsl_state)
    end
  end

  defp verify_query(listener, module, signal_fields, dsl_state) do
    read_action =
      case listener.read_action && Ash.Resource.Info.action(dsl_state, listener.read_action) do
        %{type: :read} = read_action ->
          read_action

        nil when is_nil(listener.read_action) ->
          raise_error(
            listener,
            module,
            "The resource has no primary read action, so the listener must set `read_action`."
          )

        _ ->
          raise_error(
            listener,
            module,
            "`#{inspect(listener.read_action)}` is not a read action."
          )
      end

    for {arg, _field} <- listener.args,
        !Enum.any?(read_action.arguments, &(&1.name == arg)) do
      raise_error(
        listener,
        module,
        "`#{inspect(arg)}` is not an argument of the `#{inspect(read_action.name)}` read action."
      )
    end

    for {field, _signal_field} <- listener.get_by,
        !Ash.Resource.Info.attribute(dsl_state, field) do
      raise_error(listener, module, "`get_by` field `#{inspect(field)}` is not an attribute.")
    end

    verify_fields(listener, module, :args, listener.args, signal_fields)
    verify_fields(listener, module, :get_by, listener.get_by, signal_fields)
  end

  defp verify_fields(listener, module, option, mapping, signal_fields) do
    for {_name, field} <- mapping, field not in signal_fields do
      raise_error(
        listener,
        module,
        "`#{option}` uses `#{inspect(field)}`, but signal `#{inspect(listener.signal)}` has no field with that name."
      )
    end
  end

  defp non_empty([]), do: nil
  defp non_empty(value), do: value

  defp raise_error(listener, module, message) do
    raise DslError,
      module: module,
      path: [:signals_in, :on, listener.signal],
      location: Entity.anno(listener),
      message: message
  end
end
