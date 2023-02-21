defmodule Ash.OptionsHelpers do
  @moduledoc """
  Helpers for working with nimble options
  """

  def ash_type do
    {:spark_type, Ash.Type, :builtins,
     [{"{:array, inner_type}", "list", "{:array, ${1:inner_type}}"}]}
  end

  def ash_resource do
    {:spark, Ash.Resource}
  end

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
