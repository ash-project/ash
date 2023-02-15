defmodule Ash.Resource.Transformers.DefaultAccept do
  @moduledoc "Sets the default `accept` for each action"

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  @accept_public_attributes_by_default Application.compile_env(
                                         :ash,
                                         :accept_public_attributes_by_default?
                                       )

  if is_nil(@accept_public_attributes_by_default) do
    IO.warn("""
    * IMPORTANT *
    The configuration `accept_public_attributes_by_default?` was not set.
    It is defaulting to `true` for backwards compatibility.

    This configuration must now be manually set, and it should be set to `false`.
    If you have just started a new project, set the following config:

    config :ash, :accept_public_attributes_by_default?, false

    If you are already using Ash, then you have a few options:

    1. set an explicit default_accept list for each resource that does not have one.
    2. use `default_accept :all`, which is an alias for accepting all public attributes (`writable?: false` flag is still respected)
    3. set `config :ash, :accept_public_attributes_by_default? true` to keep the current behavior.
       in 3.0, this warning will go away, and the new default behavior will be used.

    See the github issue for this for more information on why this change is being made, and
    for some code to run in `iex` to find all resources that need to be updated.

    https://github.com/ash-project/ash/issues/512
    """)
  end

  def transform(dsl_state) do
    public_attribute_names =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.reject(& &1.private?)
      |> Enum.map(& &1.name)

    default_default_accept =
      case @accept_public_attributes_by_default do
        true ->
          :all

        false ->
          []

        nil ->
          :all
      end

    default_accept =
      Transformer.get_option(
        dsl_state,
        [:actions],
        :default_accept
      ) || default_default_accept

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
