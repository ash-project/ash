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

  def make_non_root(%{custom_options: custom} = opts) do
    new_options = Keyword.put(custom, :path, custom[:path] || [])

    %{opts | custom_options: new_options}
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

  def inspect(%Ash.Filter{not: not_filter} = filter, opts) when not is_nil(not_filter) do
    if root?(opts) do
      concat([
        "#Filter<not (",
        to_doc(not_filter, make_non_root(opts)),
        ") and ",
        to_doc(%{filter | not: nil}, make_non_root(opts)),
        ">"
      ])
    else
      concat([
        "not (",
        to_doc(not_filter, make_non_root(opts)),
        ") and ",
        to_doc(%{filter | not: nil}, make_non_root(opts))
      ])
    end
  end

  def inspect(%Ash.Filter{ors: ors, relationships: relationships, attributes: attributes}, opts)
      when ors in [nil, []] and relationships in [nil, %{}] and attributes in [nil, %{}] do
    if root?(opts) do
      concat(["#Filter<", to_doc(nil, opts), ">"])
    else
      concat([to_doc(nil, opts)])
    end
  end

  def inspect(filter, opts) do
    rels =
      filter
      |> Map.get(:relationships)
      |> case do
        rels when rels == %{} ->
          []

        rels ->
          Enum.map(rels, fn {key, value} ->
            to_doc(value, add_to_path(opts, key))
          end)
      end

    attrs =
      filter
      |> Map.get(:attributes)
      |> case do
        attrs when attrs == %{} ->
          []

        attrs ->
          Enum.map(attrs, fn {key, value} ->
            to_doc(value, put_attr(opts, key))
          end)
      end

    and_container =
      case attrs ++ rels do
        [] ->
          empty()

        and_clauses ->
          Inspect.Algebra.container_doc("(", and_clauses, ")", opts, fn term, _ -> term end,
            break: :flex,
            separator: " and"
          )
      end

    all_container =
      case Map.get(filter, :ors) do
        nil ->
          and_container

        [] ->
          and_container

        ors ->
          inspected_ors = Enum.map(ors, fn filter -> to_doc(filter, make_non_root(opts)) end)

          Inspect.Algebra.container_doc(
            "",
            [and_container | inspected_ors],
            "",
            opts,
            fn term, _ -> term end,
            break: :strict,
            separator: " or "
          )
      end

    if root?(opts) do
      concat(["#Filter<", all_container, ">"])
    else
      concat([all_container])
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

defimpl Inspect, for: Ash.Filter.Or do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{left: left, right: right}, opts) do
    concat([to_doc(left, opts), " or ", to_doc(right, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.Eq do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{value: value}, opts) do
    concat([attr(opts), " == ", to_doc(value, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.NotEq do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{value: value}, opts) do
    concat([attr(opts), " != ", to_doc(value, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.In do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{values: values}, opts) do
    concat([attr(opts), " in ", to_doc(values, opts)])
  end
end

defimpl Inspect, for: Ash.Filter.NotIn do
  import Inspect.Algebra
  import Ash.Filter.InspectHelpers

  def inspect(%{values: values}, opts) do
    concat([attr(opts), " not in ", to_doc(values, opts)])
  end
end
