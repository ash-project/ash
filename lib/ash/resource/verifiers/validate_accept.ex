# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateAccept do
  @moduledoc "Validates that accept and reject lists only contain valid attributes"
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Entity
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    attributes = Ash.Resource.Info.attributes(dsl_state)

    attribute_names = MapSet.new(attributes, & &1.name)

    initial_errors = %{not_attribute: [], not_writable: []}

    result =
      Ash.Resource.Info.actions(dsl_state)
      |> Enum.reduce(%{}, fn
        %{name: action_name, accept: accept}, acc ->
          validate_attribute_name = fn attribute_name ->
            cond do
              !MapSet.member?(attribute_names, attribute_name) ->
                :not_attribute

              !Ash.Resource.Info.attribute(dsl_state, attribute_name).writable? ->
                :not_writable

              true ->
                :ok
            end
          end

          accept_errors =
            Enum.reduce(
              accept,
              initial_errors,
              fn attribute, %{not_attribute: not_attribute, not_writable: not_writable} = acc ->
                case validate_attribute_name.(attribute) do
                  :ok ->
                    acc

                  :not_writable ->
                    %{not_writable: [attribute | not_writable], not_attribute: not_attribute}

                  :not_attribute ->
                    %{not_attribute: [attribute | not_attribute], not_writable: not_writable}
                end
              end
            )

          if accept_errors == initial_errors do
            acc
          else
            Map.put(acc, action_name, %{accept: accept_errors})
          end

        _, acc ->
          acc
      end)
      |> Enum.map(fn {action_name, %{accept: accept}} ->
        accept_not_attribute = accept.not_attribute |> Enum.reverse()
        accept_not_writable = accept.not_writable |> Enum.reverse()

        # Get the action entity and try to get location info for the accept property specifically
        action_entity = Ash.Resource.Info.action(dsl_state, action_name)
        location = Entity.property_anno(action_entity, :accept)

        error_messages =
          [
            message(accept_not_attribute, "are not attributes", [:actions, action_name, :accept]),
            message(accept_not_writable, "are not writable", [:actions, action_name, :accept])
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        {error_messages, location, action_name}
      end)

    if result == [] do
      :ok
    else
      # Use the first error's location and action info
      {_first_error_msg, first_location, first_action} = hd(result)
      all_messages = Enum.map(result, fn {msg, _loc, _action} -> msg end)

      raise DslError,
        module: Spark.Dsl.Verifier.get_persisted(dsl_state, :module),
        message: Enum.join(all_messages, "\n"),
        location: first_location,
        path: [:actions, first_action, :accept]
    end
  end

  defp message(keys, _message, _path) when keys == [] do
    ""
  end

  defp message(keys, message, path) do
    dsl_path = Enum.join(path, " -> ")
    "#{dsl_path}:\n  #{get_message(keys, message)}"
  end

  defp get_message(keys, message) do
    "Cannot accept `#{inspect(keys)}` because they #{message}"
  end
end
