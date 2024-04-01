defmodule Ash.Resource.Verifiers.ValidateAccept do
  @moduledoc "Validates that accept and reject lists only contain valid attributes"
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    attributes =
      dsl_state
      |> Verifier.get_entities([:attributes])

    attribute_names = MapSet.new(attributes, & &1.name)

    initial_errors = %{not_attribute: []}

    result =
      Verifier.get_entities(dsl_state, [:actions])
      |> Enum.reduce(%{}, fn
        %{name: action_name, accept: accept}, acc ->
          validate_attribute_name = fn attribute_name ->
            if MapSet.member?(attribute_names, attribute_name) do
              :ok
            else
              :not_attribute
            end
          end

          accept_errors =
            Enum.reduce(
              accept,
              initial_errors,
              fn attribute, %{not_attribute: not_attribute} = acc ->
                case validate_attribute_name.(attribute) do
                  :ok ->
                    acc

                  :not_attribute ->
                    %{not_attribute: [attribute | not_attribute]}
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
      |> Enum.map(fn {action, %{accept: accept}} ->
        accept_not_attribute = accept.not_attribute |> Enum.reverse()

        [
          message(accept_not_attribute, "are not attributes", [:actions, action, :accept])
        ]
        |> Enum.reject(&(&1 == ""))
        |> Enum.join("\n")
      end)

    if result == [] do
      :ok
    else
      raise DslError,
        message: Enum.join(result, "\n"),
        path: []
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
    "#{inspect(keys)} #{message}"
  end
end
