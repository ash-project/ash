defmodule Ash.Flow.Info do
  @moduledoc """
  Flow introspection functions.
  """

  alias Spark.Dsl.Extension

  def description(flow) do
    Extension.get_opt(flow, [:flow], :description, "", true)
  end

  def returns(flow) do
    Extension.get_opt(flow, [:flow], :returns, nil, false)
  end

  def short_name(flow) do
    Extension.get_opt(flow, [:flow], :short_name, nil, false) || flow.default_short_name()
  end

  def trace_name(flow) do
    Extension.get_opt(flow, [:flow], :trace_name, nil, false) || to_string(short_name(flow))
  end

  def domain(flow) do
    Extension.get_opt(flow, [:flow], :domain, nil, false)
  end

  def arguments(flow) do
    Extension.get_entities(flow, [:flow])
  end

  def steps(flow) do
    Extension.get_entities(flow, [:steps])
  end
end
