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

      @all_arguments __MODULE__
                     |> Ash.Resource.Info.actions()
                     |> Enum.flat_map(& &1.arguments)
                     |> Enum.map(& &1.name)
                     |> Enum.uniq()

      @all_attributes __MODULE__
                      |> Ash.Resource.Info.attributes()
                      |> Enum.map(& &1.name)
                      |> Enum.uniq()

      @doc """
      Validates that the keys in the provided input are valid for at least one action on the resource.

      Raises a KeyError error at compile time if not. This exists because generally a struct should only ever
      be created by Ash as a result of a successful action. You should not be creating records manually in code,
      e.g `%MyResource{value: 1, value: 2}`. Generally that is fine, but often with embedded resources it is nice
      to be able to validate the keys that are being provided, e.g

      ```elixir
      Resource
      |> Ash.Changeset.for_create(:create, %{embedded: EmbeddedResource.input(foo: 1, bar: 2)})
      |> MyApp.Api.create()
      ```
      """
      def input(opts) do
        Map.new(opts, fn {key, value} ->
          if key in @all_arguments || key in @all_attributes do
            {key, value}
          else
            raise KeyError, key: key
          end
        end)
      end
    end
  end
end
