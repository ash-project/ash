# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    public_attribute_names =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.filter(&(&1.public? && &1.writable?))
      |> Enum.map(& &1.name)

    default_default_accept =
      if Ash.Resource.Info.embedded?(dsl_state) do
        :*
      else
        []
      end

    default_accept =
      Transformer.get_option(
        dsl_state,
        [:actions],
        :default_accept
      ) || default_default_accept

    dsl_state =
      Transformer.set_option(dsl_state, [:actions], :default_accept, default_accept)

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type == :action))
    |> Enum.reduce({:ok, dsl_state}, fn
      %{type: :read}, {:ok, _dsl_state} = acc ->
        acc

      %{type: :destroy, soft?: false} = action, {:ok, dsl_state} ->
        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            %{action | accept: []},
            &(&1.name == action.name && &1.type == action.type)
          )

        {:ok, new_dsl_state}

      action, {:ok, dsl_state} ->
        accept =
          List.wrap(action.accept || default_accept)
          |> Enum.flat_map(fn
            :* -> public_attribute_names
            attribute_name -> [attribute_name]
          end)

        argument_names = Enum.map(action.arguments, & &1.name)

        accept
        |> Enum.reject(&Ash.Resource.Info.attribute(dsl_state, &1))
        |> case do
          [] ->
            :ok

          invalid_attrs ->
            # Get location info from the action entity, specifically for the accept property
            action_entity = Ash.Resource.Info.action(dsl_state, action.name, action.type)

            location =
              case Entity.property_anno(action_entity, :accept) do
                nil -> Entity.anno(action_entity)
                other -> other
              end

            raise Spark.Error.DslError,
              module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
              location: location,
              path: [:actions, action.name, :accept],
              message: """
              Cannot accept #{inspect(invalid_attrs)}, because they are not attributes.
              """
        end

        action
        |> Map.get(:require_attributes, [])
        |> Enum.reject(&(&1 in accept))
        |> case do
          [] ->
            :ok

          invalid_required_attributes ->
            # Get location info from the action entity, specifically for the require_attributes property
            action_entity = Ash.Resource.Info.action(dsl_state, action.name, action.type)

            location =
              case Entity.property_anno(action_entity, :require_attributes) do
                nil -> Entity.anno(action_entity)
                other -> other
              end

            raise Spark.Error.DslError,
              module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
              location: location,
              path: [:actions, action.name, :require_attributes],
              message: """
              Cannot require #{inspect(invalid_required_attributes)}, because they are not accepted. You must accept in addition to requiring."
              """
        end

        action
        |> Map.get(:allow_nil_input, [])
        |> Enum.reject(&(&1 in accept))
        |> case do
          [] ->
            :ok

          invalid_allow_nil_inputs ->
            # Get location info from the action entity, specifically for the allow_nil_input property
            action_entity = Ash.Resource.Info.action(dsl_state, action.name, action.type)

            location =
              case Entity.property_anno(action_entity, :allow_nil_input) do
                nil -> Entity.anno(action_entity)
                other -> other
              end

            message =
              Exception.message(
                Spark.Error.DslError.exception(
                  module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
                  location: location,
                  path: [:actions, action.name, :allow_nil_input],
                  message: """
                  It is not necessary to allow nil inputs that are not accepted, got: #{inspect(invalid_allow_nil_inputs)}
                  """
                )
              )

            IO.warn(message)
        end

        accept
        |> Enum.reject(&Ash.Resource.Info.attribute(dsl_state, &1))
        |> case do
          [] ->
            :ok

          invalid_attrs ->
            # Get location info from the action entity, specifically for the accept property
            action_entity = Ash.Resource.Info.action(dsl_state, action.name, action.type)

            location =
              case Entity.property_anno(action_entity, :accept) do
                nil -> Entity.anno(action_entity)
                other -> other
              end

            raise Spark.Error.DslError,
              module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
              location: location,
              path: [:actions, action.name, :accept],
              message: """
              Cannot accept #{inspect(invalid_attrs)}, because they are not attributes.
              """
        end

        accept =
          Enum.reject(accept, &(&1 in argument_names))

        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            %{action | accept: accept},
            &(&1.name == action.name && &1.type == action.type)
          )

        {:ok, new_dsl_state}
    end)
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.CreateJoinRelationship), do: true
  def after?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def after?(_), do: false
end
