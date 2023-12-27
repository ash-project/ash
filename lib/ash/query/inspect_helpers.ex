defmodule Ash.Query.InspectHelpers do
  @moduledoc false

  # custom options not available before Elixir 1.9
  def container_type(%{custom_options: options}), do: options[:container_type]
  def container_type(_), do: nil

  def put_container_type(opts, container_type) do
    custom_options = apply(Map, :get, [opts, :custom_options])

    apply(Map, :put, [
      opts,
      :custom_options,
      Keyword.put(custom_options, :container_type, container_type)
    ])

    # above version required to avoid dialyzer warnings on lack of custom_options in pre-1.9 elixir
    # %{opts | custom_options: Keyword.put(opts.custom_options, :container_type, container_type)}
  end
end
