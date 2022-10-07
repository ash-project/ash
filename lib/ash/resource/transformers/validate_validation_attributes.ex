defmodule Ash.Resource.Transformers.ValidateValidationAttributes do
  @moduledoc """
  Validates that all built in validations point to valid attributes
  """
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(dsl) do
    attribute_names =
      dsl
      |> Transformer.get_entities([:attributes])
      |> Enum.map(& &1.name)

    dsl
    |> Transformer.get_entities([:validations])
    |> Enum.each(&validate_validation(&1, attribute_names))

    {:ok, dsl}
  end

  defp validate_validation(%Ash.Resource.Validation{} = resource_validation, attribute_names) do
    validate_validation(resource_validation.validation, attribute_names)

    Enum.each(
      resource_validation.where,
      &validate_validation(&1, attribute_names)
    )
  end

  defp validate_validation({validation_module, [{:attribute, attribute} | _]}, attribute_names) do
    check(validation_module, attribute, attribute_names)
  end

  defp validate_validation({validation_module, [{:attributes, attributes} | _]}, attribute_names) do
    Enum.each(attributes, &check(validation_module, &1, attribute_names))
  end

  defp validate_validation(_, _), do: nil

  defp check(validation_module, attribute_name, attribute_names) do
    if attribute_name not in attribute_names do
      raise Spark.Error.DslError,
        path: [:validations],
        message:
          "Validation `#{inspect(validation_module)}` expects attribute `#{attribute_name}` to be defined"
    end
  end
end
