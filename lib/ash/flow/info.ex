defmodule Ash.Flow.Info do
  @moduledoc """
  Flow introspection functions.
  """

  alias Ash.Dsl.Extension

  def description(flow) do
    Extension.get_opt(flow, [:flow], :description, "", true)
  end

  def returns(flow) do
    Extension.get_opt(flow, [:flow], :returns, nil, false)
  end

  def api(flow) do
    Extension.get_opt(flow, [:flow], :api, nil, false)
  end

  def arguments(flow) do
    Extension.get_entities(flow, [:flow])
  end

  def steps(flow) do
    Extension.get_entities(flow, [:steps])
  end
end
