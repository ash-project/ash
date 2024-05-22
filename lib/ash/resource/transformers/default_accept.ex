defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Spark.Dsl.Transformer

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
            raise Spark.Error.DslError,
              module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
              path: [:actions, action.name, :accept],
              message: """
              Cannot accept #{inspect(invalid_attrs)}, because they are not attributes."
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
