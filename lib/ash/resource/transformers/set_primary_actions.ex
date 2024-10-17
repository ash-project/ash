defmodule Ash.Resource.Transformers.SetPrimaryActions do
  @moduledoc """
  Validates the primary action configuration

  If multiple primary actions exist this results in an error.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def after?(Ash.Resource.Transformers.DefaultAccept), do: true
  def after?(_), do: false

  def transform(dsl_state) do
    with {:ok, dsl_state} <- add_defaults(dsl_state) do
      dsl_state
      |> Transformer.get_entities([:actions])
      |> Enum.group_by(& &1.type)
      |> Map.put_new(:read, [])
      |> Map.put_new(:update, [])
      |> Map.put_new(:create, [])
      |> Map.put_new(:destroy, [])
      |> Enum.reduce_while({:ok, dsl_state}, fn
        {_type, []}, {:ok, dsl_state} ->
          {:cont, {:ok, dsl_state}}

        {type, actions}, {:ok, dsl_state} ->
          case Enum.count_until(actions, & &1.primary?, 2) do
            2 ->
              {:halt,
               {:error,
                DslError.exception(
                  message:
                    "Multiple actions of type #{type} configured as `primary?: true`, but only one action per type can be the primary",
                  path: [:actions, type]
                )}}

            _ ->
              {:cont, {:ok, dsl_state}}
          end
      end)
    end
  end

  defp add_defaults(dsl_state) do
    actions = Transformer.get_entities(dsl_state, [:actions])

    default_defaults =
      if Transformer.get_persisted(dsl_state, :embedded?) do
        [{:create, :*}, :read, {:update, :*}, :destroy]
        |> Enum.reject(fn action_name ->
          Enum.any?(actions, &(&1.name == action_name))
        end)
        |> Enum.reverse()
      else
        []
      end

    default_accept =
      List.wrap(
        Transformer.get_option(
          dsl_state,
          [:actions],
          :default_accept
        )
      )

    dsl_state
    |> Transformer.get_option([:actions], :defaults)
    |> Kernel.||(default_defaults)
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, dsl_state}, fn {type_and_accept, i}, {:ok, dsl_state} ->
      {type, accept} =
        case type_and_accept do
          {type, _accept} when type in [:destroy, :read] ->
            raise Spark.Error.DslError,
              module: Spark.Dsl.Transformer.get_persisted(dsl_state, :module),
              path: [:actions, :default_accept],
              message: """
              Do not specify an accept for the default actions of type `:read` and `:destroy`.

              Place them at the beginning of the list if you need to avoid specifying a value for them:

                  defaults [:read, :destroy, create: [...], update: [...]]
              """

          type when type in [:create, :update] ->
            {type, default_accept}

          {type, accept} ->
            {type, List.wrap(accept)}

          type ->
            {type, []}
        end

      unless type in [:create, :update, :read, :destroy] do
        raise Spark.Error.DslError,
          path: [:actions, :defaults, i],
          message: "#{type} is not a valid action type"
      end

      primary? = !Enum.any?(actions, &(&1.type == type && &1.primary?))

      transactions_enabled? = Ash.DataLayer.can?(:transact, dsl_state)

      if type == :read do
        Ash.Resource.Builder.prepend_action(dsl_state, type, type,
          primary?: primary?,
          transaction?: false,
          pagination:
            Ash.Resource.Builder.build_pagination(
              required?: false,
              offset?: true,
              keyset?: true,
              countable: Ash.DataLayer.data_layer_can?(dsl_state, {:query_aggregate, :count})
            )
        )
      else
        opts =
          if type in [:update, :destroy] do
            [
              require_atomic?: false,
              primary?: primary?,
              accept: accept,
              transaction?: transactions_enabled?
            ]
          else
            [
              primary?: primary?,
              accept: accept,
              transaction?: transactions_enabled?
            ]
          end

        Ash.Resource.Builder.prepend_action(dsl_state, type, type, opts)
      end
      |> case do
        {:ok, dsl_state} -> {:cont, {:ok, dsl_state}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end
end
