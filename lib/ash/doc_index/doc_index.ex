defmodule Ash.DocIndex do
  @moduledoc """
  A module for configuring how a library is rendered in ash_hq.

  There is a small template syntax available in any documentation.
  The template syntax will link to the user's currently selected
  version for the relevant library.

  All templates should be wrapped in double curly braces, e.g `{{}}`. This is not done here
  as it would be replaced in the very documentation you are reading.

  ## Links to other documentation

  `link:library:item_type:name`

  The only item_type currently supported is `guide`, so the name would be `category/name`

  For example:

  `link:ash:guide:Topics/Attributes` -> `-> `<a href="/docs/guides/ash/topics/attributes.md">Attributes</a>`

  ## Mix dependencies

  `mix_dep:library`

  For example:

  `mix_dep:ash` -> `{:ash, "~> 1.5"}`
  """

  @type extension :: %{
          optional(:module) => module,
          optional(:target) => String.t(),
          optional(:default_for_target?) => boolean,
          :name => String.t(),
          :type => String.t()
        }

  @type guide :: %{
          name: String.t(),
          text: String.t(),
          category: String.t() | nil,
          route: String.t() | nil
        }

  @callback extensions() :: list(extension())
  @callback for_library() :: String.t()
  @callback guides() :: list(guide())
  @callback code_modules() :: [{String.t(), list(module())}]

  defmacro __using__(opts) do
    quote bind_quoted: [guides_from: opts[:guides_from]] do
      @behaviour Ash.DocIndex

      if guides_from do
        @files guides_from
               |> Path.wildcard()
               |> Enum.map(fn path ->
                 path
                 |> Path.split()
                 |> Enum.drop(1)
                 |> case do
                   [category, file] ->
                     %{
                       name: Ash.DocIndex.to_name(Path.rootname(file)),
                       category: Ash.DocIndex.to_name(category),
                       text: File.read!(path),
                       route: "#{Ash.DocIndex.to_path(category)}/#{Ash.DocIndex.to_path(file)}"
                     }
                 end
               end)

        @impl Ash.DocIndex
        def guides do
          @files
        end

        defoverridable guides: 0
      end
    end
  end

  def to_name(string) do
    string
    |> String.split(~r/[-_]/, trim: true)
    |> Enum.map_join(" ", &String.capitalize/1)
  end

  def to_path(string) do
    string
    |> String.split(~r/\s/, trim: true)
    |> Enum.join("-")
    |> String.downcase()
  end
end
