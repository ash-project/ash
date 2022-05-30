defmodule Ash.DocIndex do
  @moduledoc """
  A module for configuring how a library is rendered in ash_hq
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
  @callback code_modules() :: list(module())

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
      end
    end
  end

  def ensure_documented!(doc_index) do
    Enum.each(doc_index.extensions(), fn extension ->
      Enum.each(extension.module().sections(), &do_ensure_documented!/1)
    end)
  end

  defp do_ensure_documented!(
         section_or_entity,
         trail \\ []
       )

  defp do_ensure_documented!(
         %Ash.Dsl.Section{name: name, schema: schema, entities: entities, sections: sections},
         trail
       ) do
    case Enum.find(schema, fn {_key, opts} ->
           !opts[:links]
         end) do
      {key, _opts} ->
        opt =
          trail
          |> Enum.reverse([key])
          |> Enum.join(".")

        raise "#{opt} is not documented (must contain links)!"

      _ ->
        :ok
    end

    Enum.each(entities, &do_ensure_documented!(&1, [name | trail]))
    Enum.each(sections, &do_ensure_documented!(&1, [name | trail]))
  end

  defp do_ensure_documented!(
         %Ash.Dsl.Entity{name: name, schema: schema, entities: entities},
         trail
       ) do
    case Enum.find(schema, fn {_key, opts} ->
           !opts[:links]
         end) do
      {key, _opts} ->
        opt =
          trail
          |> Enum.reverse([key])
          |> Enum.join(".")

        raise "#{opt} is not documented (must contain links)!"

      _ ->
        :ok
    end

    Enum.each(entities, &do_ensure_documented!(elem(&1, 1), [name | trail]))
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
