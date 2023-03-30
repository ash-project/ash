defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    public_attribute_names =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.reject(& &1.private?)
      |> Enum.map(& &1.name)

    default_accept =
      Transformer.get_option(
        dsl_state,
        [:actions],
        :default_accept
      ) || public_attribute_names

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reduce({:ok, dsl_state}, fn
      %{type: :read}, {:ok, _dsl_state} = acc ->
        acc

      action, {:ok, dsl_state} ->
        if is_list(action.accept) && is_list(action.reject) &&
             !MapSet.disjoint?(MapSet.new(action.accept), MapSet.new(action.reject)) do
          raise Spark.Error.DslError,
            path: [:actions, action.name],
            message: "accept and reject keys cannot overlap"
        end

        {accept, reject} =
          case {action.accept, action.reject} do
            {_, :all} ->
              {[], public_attribute_names}

            {nil, reject} ->
              {reject(if(action.type != :destroy, do: default_accept, else: []), reject), reject}

            {:all, reject} ->
              {reject(public_attribute_names, reject), reject}

            {accept, reject} ->
              {reject(accept, reject), reject}
          end

        accept =
          Enum.reject(accept, fn attr ->
            Enum.any?(action.arguments, &(&1.name == attr))
          end)

        new_dsl_state =
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            %{action | accept: accept, reject: reject},
            &(&1.name == action.name && &1.type == action.type)
          )

        {:ok, new_dsl_state}
    end)
  end

  defp reject(list, reject) do
    Enum.reject(list, &(&1 in reject))
  end

  def after?(Ash.Resource.Transformers.BelongsToSourceField), do: true
  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.CreateJoinRelationship), do: true
  def after?(Ash.Resource.Transformers.ValidatePrimaryActions), do: true
  def after?(_), do: false
end
