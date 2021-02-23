defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.Dsl`
  """

  @type t :: module
  @type record :: struct()

  use Ash.Dsl,
    single_extension_kinds: [:data_layer],
    many_extension_kinds: [
      :authorizers,
      :notifiers
    ],
    default_extensions: [
      data_layer: Module.concat(["Ash", DataLayer, Simple]),
      extensions: [Module.concat(["Ash", Resource, Dsl])]
    ]

  def init(opts) do
    if opts[:data_layer] == :embedded do
      {:ok,
       opts
       |> Keyword.put(:data_layer, Module.concat(["Ash", DataLayer, Simple]))
       |> Keyword.put(:embedded?, true)}
    else
      {:ok, opts}
    end
  end

  def handle_opts(opts) do
    quote bind_quoted: [embedded?: opts[:embedded?]] do
      if embedded? do
        @persist {:embedded?, true}

        require Ash.EmbeddableType

        Ash.EmbeddableType.define_embeddable_type()
      end
    end
  end

  def handle_before_compile(_opts) do
    quote do
      require Ash.Schema

      Ash.Schema.define_schema()
    end
  end
end
