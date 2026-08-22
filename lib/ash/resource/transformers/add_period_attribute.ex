# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.AddPeriodAttribute do
  # Adds or checks the period attribute of a temporal resource
  @moduledoc false
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(_), do: false

  def transform(dsl_state) do
    if Ash.Resource.Info.temporal?(dsl_state) do
      add_or_check(dsl_state)
    else
      {:ok, dsl_state}
    end
  end

  defp add_or_check(dsl_state) do
    attribute_name = Ash.Resource.Info.temporal_attribute(dsl_state)
    module = Transformer.get_persisted(dsl_state, :module)

    case Ash.Resource.Info.attribute(dsl_state, attribute_name) do
      nil ->
        Ash.Resource.Builder.add_attribute(dsl_state, attribute_name, Ash.Type.Range,
          allow_nil?: false,
          generated?: true,
          constraints: [
            inner_type: :datetime,
            lower: [inclusive?: true],
            upper: [inclusive?: false]
          ]
        )

      attribute ->
        check(attribute, module)
        {:ok, mark_generated(dsl_state, attribute)}
    end
  end

  defp mark_generated(dsl_state, %{generated?: true}), do: dsl_state

  defp mark_generated(dsl_state, attribute) do
    Transformer.replace_entity(
      dsl_state,
      [:attributes],
      %{attribute | generated?: true},
      &(&1.name == attribute.name)
    )
  end

  defp check(attribute, module) do
    cond do
      attribute.type != Ash.Type.Range ->
        raise Spark.Error.DslError,
          module: module,
          path: [:attributes, attribute.name],
          message: """
          Expected the attribute #{attribute.name} to be an `Ash.Type.Range`, since it is this \
          resource's period. Got #{inspect(attribute.type)}.
          """

      not inclusive_exclusive?(attribute.constraints) ->
        raise Spark.Error.DslError,
          module: module,
          path: [:attributes, attribute.name],
          message: """
          Expected the attribute #{attribute.name} to constrain its bounds to \
          `lower: [inclusive?: true], upper: [inclusive?: false]`. Periods must all take one \
          form for adjacent ones to meet without overlapping or leaving a gap. \
          #{bounds_got(attribute)}.
          """

      attribute.allow_nil? ->
        raise Spark.Error.DslError,
          module: module,
          path: [:attributes, attribute.name],
          message: """
          Expected the attribute #{attribute.name} not to be `allow_nil? true`. A record of a \
          temporal resource is valid over some period, and one valid over no period cannot be \
          read at any point in time.
          """

      true ->
        :ok
    end
  end

  defp inclusive_exclusive?(constraints) do
    constraints[:lower][:inclusive?] == true and constraints[:upper][:inclusive?] == false
  end

  defp bounds_got(%{constraints: constraints}) do
    case {constraints[:lower][:inclusive?], constraints[:upper][:inclusive?]} do
      {nil, nil} -> "It constrains neither bound"
      {lower, upper} -> "Got lower inclusive? #{inspect(lower)}, upper #{inspect(upper)}"
    end
  end
end
