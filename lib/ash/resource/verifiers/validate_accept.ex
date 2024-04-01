defmodule Ash.Resource.Verifiers.ValidateAccept do
  @moduledoc "Validates that accept and reject lists only contain valid attributes"
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    {public_attributes, private_attributes} =
      dsl_state
      |> Verifier.get_entities([:attributes])
      |> Enum.split_with(& &1.public?)

    public_attribute_names = MapSet.new(public_attributes, & &1.name)
    private_attribute_names = MapSet.new(private_attributes, & &1.name)

    initial_errors = %{private: [], not_attribute: []}

    result =
      Verifier.get_entities(dsl_state, [:actions])
      |> Enum.reduce(%{}, fn
        %{name: action_name, accept: accept}, acc ->
          validate_attribute_name = fn attribute_name ->
            cond do
              MapSet.member?(private_attribute_names, attribute_name) ->
                :private

              MapSet.member?(public_attribute_names, attribute_name) ->
                :ok

              true ->
                :not_attribute
            end
          end

          accept_errors =
            Enum.reduce(
              accept,
              initial_errors,
              fn attribute, %{private: private, not_attribute: not_attribute} = acc ->
                case validate_attribute_name.(attribute) do
                  :ok ->
                    acc

                  :private ->
                    %{private: [attribute | private], not_attribute: not_attribute}

                  :not_attribute ->
                    %{private: private, not_attribute: [attribute | not_attribute]}
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
