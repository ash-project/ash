# Feel free to improve this!
defmodule Ash.Filter.InspectHelpers do
  def attr(opts) do
    case path(opts) do
      path when path in [nil, []] ->
        to_string(get_attr(opts))

      path ->
        Enum.join(path || [], ".") <> "." <> to_string(get_attr(opts))
    end
  end

  def root?(opts) do
    path(opts) == nil && get_attr(opts) == nil
  end

  def put_attr(%{custom_options: custom} = opts, attr) do
    %{opts | custom_options: Keyword.put(custom, :attr, attr)}
  end

  def add_to_path(%{custom_options: custom} = opts, path_item) do
    new_options =
      Keyword.update(custom, :path, [to_string(path_item)], fn path ->
        [path_item | path]
      end)

    %{opts | custom_options: new_options}
  end

  defp get_attr(opts) do
    opts.custom_options[:attr]
  end

  defp path(opts) do
    opts.custom_options[:path]
  end
end

defimpl Inspect, for: Ash.Filter do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{or: nil} = filter, opts) do
    rels =
      filter
      |> Map.get(:relationships)
      |> Enum.map(fn {key, value} ->
        to_doc(value, add_to_path(opts, key))
      end)

    attrs =
      filter
      |> Map.get(:attributes)
      |> Enum.map(fn {key, value} ->
        to_doc(value, put_attr(opts, key))
      end)
      |> Enum.concat(rels)
      |> Enum.intersperse(" and ")
      |> concat()

    if root?(opts) do
      concat(["#Filter< ", attrs, " >"])
    else
      concat([attrs])
    end
  end

  def inspect(%{or: or_filter} = filter, opts) do
    filter_without_or = %{filter | or: nil}

    if root?(opts) do
      concat([
        "#Ash.Filter<(",
        to_doc(filter_without_or, opts),
        ") or (",
        to_doc(or_filter, opts),
        ")>"
      ])
    else
      concat(["(", to_doc(filter_without_or, opts), ") or (", to_doc(or_filter, opts), ")"])
    end
  end
end

defimpl Inspect, for: Ash.Filter.And do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{left: left, right: right}, opts) do
    concat([to_doc(left, opts), " and ", to_doc(right, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.Eq do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{value: value}, opts) do
    concat([attr(opts), " == ", to_doc(value, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.In do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{values: values}, opts) do
    concat([attr(opts), " in ", to_doc(values, opts)])
  end
end
