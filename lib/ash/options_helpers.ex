defmodule Ash.OptionsHelpers do
  @moduledoc """
  Helpers for working with nimble options
  """

  @doc false
  @spec ash_type() :: {:spark_type, Ash.Type, :builtins, [{String.t(), String.t(), String.t()}]}
  def ash_type do
    {:spark_type, Ash.Type, :builtins,
     [{"{:array, inner_type}", "list", "{:array, ${1:inner_type}}"}]}
  end

  @doc false
  @spec ash_resource() :: {:spark, Ash.Resource}
  def ash_resource do
    {:spark, Ash.Resource}
  end

  @doc false
  @spec hide_all_except(Keyword.t(), [atom()]) :: Keyword.t()
  def hide_all_except(options, keys) do
    Enum.map(options, fn {key, config} ->
      if key in keys do
        {key, config}
      else
        {key, Keyword.put(config, :hide, true)}
      end
    end)
  end
end
