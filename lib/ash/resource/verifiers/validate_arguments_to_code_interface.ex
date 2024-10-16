defmodule Ash.Resource.Verifiers.ValidateArgumentsToCodeInterface do
  @moduledoc """
  Validate the arguments defined in the code interface
  and reject arguments that are not action attributes/arguments
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
          "as args because they are not defined in the `accept` list of the `#{inspect(action.name)}` action",
          [:code_interface, interface_name]
        ),
        message(
          "args",
          arguments_not_supported,
          "because they are not arguments or attributes supported by the `#{inspect(action.name)}` action",
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

  defp to_non_optional_arg({:optional, interface_arg}), do: interface_arg

  defp to_non_optional_arg(interface_arg), do: interface_arg

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
