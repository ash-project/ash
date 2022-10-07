defmodule Ash.Resource.Transformers.ValidateAcceptRejectOverlap do
  @moduledoc """
  Validate that an actions accept and reject properties are mutually exclusive (do not overlap).
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  @impl true
  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(_), do: true

  @impl true
  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.each(fn
      %{name: action_name, accept: accept, reject: reject} when is_list(accept) and is_list(reject) ->
        unless MapSet.disjoint?(MapSet.new(accept), MapSet.new(reject)) do
          raise DslError,
          path: [:actions, action_name],
          message: "accept and reject keys cannot overlap"
        end
      _ ->
        :ok
    end)

    :ok
  end
end
