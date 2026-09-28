# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.AddPeriodAttribute do
  # Adds or checks the period attribute of a temporal resource, and adds its `recorded_at`
  # attribute if it names one it doesn't declare
  @moduledoc false
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(_), do: false

  def transform(dsl_state) do
    if Ash.Resource.Info.temporal?(dsl_state) do
      with {:ok, dsl_state} <- add_or_check(dsl_state) do
        add_recorded_at(dsl_state)
      end
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
            inner_type: :utc_datetime_usec,
            lower: [inclusive?: true],
            upper: [inclusive?: false]
          ]
        )

      attribute ->
        check(attribute, module)
        {:ok, mark_generated(dsl_state, attribute)}
    end
  end

  defp add_recorded_at(dsl_state) do
    with name when not is_nil(name) <- Ash.Resource.Info.temporal_recorded_at(dsl_state),
         nil <- Ash.Resource.Info.attribute(dsl_state, name) do
      Ash.Resource.Builder.add_attribute(dsl_state, name, :utc_datetime_usec,
        allow_nil?: false,
        writable?: false,
        default: &DateTime.utc_now/0,
        update_default: &DateTime.utc_now/0
      )
    else
      _ -> {:ok, dsl_state}
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

      not datetime_inner_type?(attribute.constraints) ->
        raise Spark.Error.DslError,
          module: module,
          path: [:attributes, attribute.name],
          message: """
          Expected the attribute #{attribute.name} to be a range over datetimes, i.e \
          `inner_type: :utc_datetime_usec`, `:utc_datetime` or `:datetime`, since it is this \
          resource's period. Got #{inspect(attribute.constraints[:inner_type])}.
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

  @datetime_types [Ash.Type.DateTime, Ash.Type.UtcDatetime, Ash.Type.UtcDatetimeUsec]

  defp datetime_inner_type?(constraints) do
    type = Ash.Type.get_type(constraints[:inner_type])

    type =
      if Ash.Type.NewType.new_type?(type), do: Ash.Type.NewType.subtype_of(type), else: type

    type in @datetime_types
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
