defmodule Ash.Flow.StepHelpers do
  @moduledoc "Template functions available while declaring steps."

  @doc "Accesses a flow argument"
  def arg(name) do
    {:_arg, name}
  end

  @doc "Accesses the result of a step"
  def result(step) do
    {:_result, step}
  end

  @doc "Accesses a path in a value lazily. Supports the value being a template, e.g `path(result(:foo), [:bar, 0, :baz])`"
  def path(template, path) do
    {:_path, template, List.wrap(path)}
  end

  @doc "Creates a range lazily. Supports the start or finish being a template, e.g `range(result(:foo), result(:bar))`"
  def range(start, finish) do
    {:_range, start, finish}
  end

  @doc "Accesses the value being iterated over for a given map step. The name is required so that `map` steps can be nested."
  def element(element) do
    {:_element, element}
  end
end
