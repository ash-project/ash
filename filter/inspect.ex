defmodule Ash.Filter.InspectHelpers do
  @moduledoc false
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
