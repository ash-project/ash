defmodule Ash.Resource.Transformers.ValidateAccept do
  @moduledoc "Validates that accept and reject lists only contains valid attributes"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(dsl_state) do
    {private_attributes, public_attributes} =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.split_with(& &1.private?)

    public_attribute_names = MapSet.new(public_attributes, & &1.name)
    private_attribute_names = MapSet.new(private_attributes, & &1.name)

    Transformer.get_entities(dsl_state, [:actions])
    |> Enum.each(fn
      %{name: action_name, accept: accept, reject: reject} ->
        validate_attribute_name = fn attribute_name, type ->
          cond do
            MapSet.member?(private_attribute_names, attribute_name) ->
              raise DslError,
                path: [:actions, action_name, type, attribute_name],
                message: "#{attribute_name} is a private attribute"

            MapSet.member?(public_attribute_names, attribute_name) ->
              :ok

            true ->
              raise DslError,
                path: [:actions, action_name, type, attribute_name],
                message: "#{attribute_name} is not an attribute"
          end
        end

        Enum.each(
          accept,
          &validate_attribute_name.(
            &1,
            :accept
          )
        )

        Enum.each(
          reject,
          &validate_attribute_name.(
            &1,
            :reject
          )
        )

      # read types do not have accept / reject fields
      %{type: :read} ->
        :ok
    end)

    :ok
  end
end
