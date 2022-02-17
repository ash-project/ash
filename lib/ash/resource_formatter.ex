defmodule Ash.ResourceFormatter do
  @moduledoc """
  Formats Ash resources. WARNING: This is untested, use at your own risk! *do not run without commiting your code first*!

  Currently, it is very simple, and will only reorder the outermost sections according to some rules.

  # Plugin

  Include the plugin into your `.formatter.exs` like so `plugins: [Ash.ResourceFormatter]`.

  If no configuration is provided, it will sort all top level DSL sections *alphabetically*.

  # Section Order

  To provide a custom section order (for both ash and any extensions), add configuration to your app, for example:

  ```elixir
  config :ash, :formatter,
    section_order: [
      :resource,
      :postgres,
      :attributes,
      :relationships,
      :aggregates,
      :calculations
    ]
  ```

  Any sections found in your resource that aren't in that list will be left in the order that they were in, the sections
  in the list will be sorted "around" those sections. E.g the following list: `[:code_interface, :attributes]` can be interpreted as
  "ensure that code_interface comes before attributes, and don't change the rest".

  # Using something other than `Ash.Resource`

  The resource formatter triggers when it sees a `use Ash.Resource` in a module.  In some cases, you may be
  "use"ing a different module, e.g `use MyApp.Resource`. To support this, you can configure the `using_modules`, like so:

  ```elixir
  config :ash, :formatter,
    using_modules: [Ash.Resource, MyApp.Resource]
  ```

  """
  @behaviour Mix.Tasks.Format

  require Logger

  def features(_opts) do
    [extensions: [".ex", ".exs"]]
  end

  def format(contents, opts) do
    using_modules = Application.get_env(:ash, :formatter)[:using_modules] || [Ash.Resource]

    contents
    |> reorder_sections(opts, using_modules)
    |> Code.format_string!(opts_without_plugin(opts))
    |> then(fn iodata ->
      [iodata, ?\n]
    end)
    |> IO.iodata_to_binary()
  rescue
    e ->
      Logger.info("Exception while formatting: #{Exception.format(:error, e, __STACKTRACE__)}")

      contents
  end

  # Due to issues around generating a list of patches to apply and applying them all at once, this now iteratively swaps sections until no more sections
  # need to be swapped. It is probably way slower than a more efficient alternative, but its the only non-buggy implementation so far.
  # An alternative may be to use a depth-tracker (via a custom AST traversal function) and do the "deepest" changes first, or group them by depth, and make n
  # calls to `patch_string/2` where n is each depth that has patches, starting at the highest depth.
  defp reorder_sections(contents, opts, using_modules) do
    contents
    |> Sourceror.parse_string!()
    |> get_reorder_patch(opts, using_modules)
    |> case do
      [] ->
        contents

      patches ->
        contents
        |> Sourceror.patch_string(patches)
        |> reorder_sections(opts, using_modules)
    end
  end

  defp get_reorder_patch(parsed, opts, using_modules) do
    {_, patches} =
      Macro.prewalk(parsed, [], fn
        {:defmodule, _, [_, [{{:__block__, _, [:do]}, {:__block__, _, body}}]]} = expr, [] ->
          case get_extensions(body, using_modules) do
            {:ok, extensions} ->
              replacement = do_reorder_sections(body, extensions)

              section_moves = body |> Enum.zip(replacement)

              patches =
                Enum.find_value(section_moves, fn {body_section, replacement_section} ->
                  if body_section != replacement_section do
                    move_to =
                      Enum.find_value(section_moves, fn {left, _} ->
                        if left == replacement_section do
                          left
                        end
                      end)

                    [
                      %{
                        range: Sourceror.get_range(body_section, include_comments: true),
                        change: Sourceror.to_string(replacement_section, opts)
                      },
                      %{
                        range: Sourceror.get_range(move_to, include_comments: true),
                        change: Sourceror.to_string(body_section)
                      }
                    ]
                  end
                end)

              {expr, patches || []}

            _ ->
              {expr, []}
          end

        expr, patches ->
          {expr, patches}
      end)

    patches
  end

  defp do_reorder_sections(body, extensions) do
    sections =
      [Ash.Resource.Dsl | extensions]
      |> Enum.flat_map(fn extension ->
        Enum.map(extension.sections(), fn section ->
          {extension, section}
        end)
      end)
      |> sort_sections()

    section_names = Enum.map(sections, fn {_, section} -> section.name end)

    {section_exprs, non_section_exprs} =
      body
      |> Enum.split_with(fn {name, _, _} ->
        name in section_names
      end)

    new_sections =
      section_names
      |> Enum.flat_map(fn section_name ->
        matching_section =
          Enum.find(section_exprs, fn
            {^section_name, _, _} -> true
            _ -> nil
          end)

        case matching_section do
          nil ->
            []

          section_expr ->
            [section_expr]
        end
      end)

    Enum.concat(non_section_exprs, new_sections)
  end

  defp sort_sections(sections) do
    case Application.get_env(:ash, :formatter)[:section_order] do
      nil ->
        Enum.sort_by(sections, fn {_extension, section} ->
          section.name
        end)

      section_order ->
        {ordered, unordered} =
          sections
          |> Enum.with_index()
          |> Enum.split_with(fn {{_, section}, _} ->
            section.name in section_order
          end)

        reordered =
          ordered
          |> Enum.map(&elem(&1, 0))
          |> Enum.sort_by(fn {_, section} ->
            Enum.find_index(section_order, &(&1 == section.name))
          end)

        Enum.reduce(unordered, reordered, fn {{extension, section}, i}, acc ->
          List.insert_at(acc, i, {extension, section})
        end)
    end
  end

  defp get_extensions(body, using_modules) do
    Enum.find_value(body, :error, fn
      {:use, _, using} ->
        [using, opts] =
          case Ash.Dsl.Extension.expand_alias(using, __ENV__) do
            [using] ->
              [using, []]

            [using, opts] ->
              [using, opts]
          end

        if using in using_modules do
          {:ok, parse_extensions(opts)}
        end

      _ ->
        nil
    end)
  end

  defp parse_extensions(blocks) do
    blocks
    |> Enum.flat_map(fn {{:__block__, _, _}, extensions} ->
      extensions
      |> case do
        {:__block__, _, [extensions]} ->
          extensions

        extension when is_atom(extension) ->
          extension

        _ ->
          []
      end
      |> List.wrap()
      |> Enum.flat_map(fn extension ->
        case Code.ensure_compiled(extension) do
          {:module, module} ->
            if Ash.Helpers.implements_behaviour?(module, Ash.Dsl.Extension) do
              [module]
            else
              []
            end

          _ ->
            []
        end
      end)
    end)
  end

  defp opts_without_plugin(opts) do
    Keyword.update(opts, :plugins, [], &(&1 -- [__MODULE__]))
  end
end
