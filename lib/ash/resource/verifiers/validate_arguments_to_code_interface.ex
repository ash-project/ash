# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateArgumentsToCodeInterface do
  @moduledoc """
  Validate the arguments defined in the code interface
  and reject arguments that are not action attributes/arguments,
  or, for calculation interfaces, not calculation arguments or resource fields
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl) do
    dsl
    |> Ash.Resource.Info.interfaces()
    |> Enum.each(fn interface ->
      action = Ash.Resource.Info.action(dsl, interface.action || interface.name)

      attributes = Verifier.get_entities(dsl, [:attributes])

      module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

      verify_interface!(interface, action, attributes, module)
    end)

    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    dsl
    |> Ash.Resource.Info.calculation_interfaces()
    |> Enum.each(&verify_calculation_interface!(&1, dsl, module))

    :ok
  end

  def verify_interface!(
        interface,
        action,
        attributes,
        module
      ) do
    interface_name = interface.action || interface.name

    interface_args = MapSet.new(interface.args || [])

    attribute_names = MapSet.new(attributes, & &1.name)
    action_arguments = MapSet.new(action.arguments, & &1.name)

    action_accepts = get_in(action, [Access.key(:accept)])

    action_supports_accept? = action_accepts != nil

    action_accepts = MapSet.new(action_accepts || [])

    custom_input_names = MapSet.new(interface.custom_inputs, & &1.name)

    errors =
      interface_args
      |> Enum.reduce(
        %{
          not_writable: [],
          not_input: []
        },
        fn interface_arg, errors ->
          interface_arg = to_non_optional_arg(interface_arg)

          cond do
            interface_arg in custom_input_names ->
              errors

            is_attribute?(attribute_names, interface_arg) &&
              not is_argument?(action_arguments, interface_arg) &&
              not is_accepted?(action_accepts, interface_arg) &&
                action_supports_accept? ->
              Map.update(errors, :not_writable, [interface_arg], &[interface_arg | &1])

            is_attribute?(attribute_names, interface_arg) ->
              errors

            is_argument?(action_arguments, interface_arg) ->
              errors

            true ->
              Map.update(errors, :not_input, [interface_arg], &[interface_arg | &1])
          end
        end
      )

    attribute_not_writable = Enum.reverse(errors.not_writable)
    arguments_not_supported = Enum.reverse(errors.not_input)

    result =
      [
        message(
          "attributes",
          attribute_not_writable,
          "as args because they are not defined in the `accept` list of the `#{inspect(action.name)}` action, and are not listed as a custom input",
          [:code_interface, interface_name]
        ),
        message(
          "args",
          arguments_not_supported,
          "because they are not arguments or attributes supported by the `#{inspect(action.name)}` action, and are not listed as a custom input",
          [:code_interface, interface_name]
        )
      ]
      |> Enum.reject(&(&1 == ""))

    if result == [] do
      :ok
    else
      raise DslError,
        module: module,
        message: Enum.join(result, "\n"),
        path: []
    end
  end

  def verify_calculation_interface!(interface, resource, module) do
    case Ash.Resource.Info.calculation(resource, interface.calculation) do
      nil ->
        :ok

      calculation ->
        argument_names = MapSet.new(calculation.arguments, & &1.name)
        field_names = resource |> Ash.Resource.Info.fields() |> MapSet.new(& &1.name)
        custom_input_names = MapSet.new(interface.custom_inputs, & &1.name)

        invalid_args =
          interface.args
          |> List.wrap()
          |> Enum.map(&to_non_optional_calc_arg/1)
          |> Enum.reject(&valid_calc_arg?(&1, argument_names, field_names, custom_input_names))

        if invalid_args == [] do
          :ok
        else
          raise DslError,
            module: module,
            message:
              message(
                "args",
                invalid_args,
                "because they are not arguments of the `#{inspect(interface.calculation)}` calculation or fields on the resource, and are not listed as a custom input",
                [:code_interface, interface.name]
              ),
            path: []
        end
    end
  end

  defp to_non_optional_arg({:optional, interface_arg}), do: interface_arg

  defp to_non_optional_arg(interface_arg), do: interface_arg

  defp to_non_optional_calc_arg({:optional, interface_arg}), do: interface_arg
  defp to_non_optional_calc_arg({:optional, interface_arg, _default}), do: interface_arg
  defp to_non_optional_calc_arg(interface_arg), do: interface_arg

  defp valid_calc_arg?(:_record, _argument_names, _field_names, _custom_input_names), do: true

  defp valid_calc_arg?({:arg, name}, argument_names, _field_names, _custom_input_names) do
    MapSet.member?(argument_names, name)
  end

  defp valid_calc_arg?({:ref, name}, _argument_names, field_names, _custom_input_names) do
    MapSet.member?(field_names, name)
  end

  defp valid_calc_arg?(name, argument_names, field_names, custom_input_names) do
    MapSet.member?(argument_names, name) or MapSet.member?(field_names, name) or
      MapSet.member?(custom_input_names, name)
  end

  defp is_attribute?(action_attributes, interface_arg) do
    MapSet.member?(action_attributes, interface_arg)
  end

  defp is_accepted?(action_accepts, interface_arg) do
    MapSet.member?(action_accepts, interface_arg)
  end

  defp is_argument?(action_arguments, interface_arg) do
    MapSet.member?(action_arguments, interface_arg)
  end

  defp message(_type, keys, _message, _path) when keys == [] do
    ""
  end

  defp message(type, keys, message, path) do
    dsl_path = Enum.join(path, " -> ")
    "#{dsl_path}:\n  #{get_message(type, keys, message)}"
  end

  defp get_message(type, keys, message) do
    "Cannot accept the #{type} `#{inspect(keys)}` #{message}"
  end
end
