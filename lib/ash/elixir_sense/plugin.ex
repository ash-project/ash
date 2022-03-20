if Code.ensure_loaded?(ElixirSense.Plugin) do
  defmodule Ash.ElixirSense.Plugin do
    @moduledoc false

    @behaviour ElixirSense.Plugin
    use ElixirSense.Providers.Suggestion.GenericReducer

    alias Ash.ElixirSense.Resource
    alias Ash.ElixirSense.Types
    alias ElixirSense.Providers.Suggestion.Matcher

    def suggestions(hint, {_, function_call, arg_index, info}, _chain, opts) do
      option = info.option || get_option(opts.cursor_context.text_before)

      if option do
        get_suggestions(hint, opts, [function_call], {:value, option})
      else
        get_suggestions(hint, opts, [function_call], {:arg, arg_index})
      end
    end

    def suggestions(hint, opts) do
      option = get_section_option(opts.cursor_context.text_before)

      if option do
        get_suggestions(hint, opts, [], {:value, option})
      else
        get_suggestions(hint, opts)
      end
    end

    def get_suggestions(hint, opts, opt_path \\ [], type \\ nil) do
      with true <- Enum.any?(opts.env.attributes, &(&1.name == :ash_is)),
           dsl_mod when not is_nil(dsl_mod) <-
             Enum.find(opts.env.requires, &ash_extension?/1) do
        extension_kinds =
          List.flatten(dsl_mod.module_info[:attributes][:ash_extension_kinds] || [])

        extensions = default_extensions(dsl_mod) ++ parse_extensions(opts, extension_kinds)

        scopes_to_lines =
          Enum.reduce(opts.buffer_metadata.lines_to_env, %{}, fn {line, env}, acc ->
            line = line - 1

            Map.update(acc, env.scope_id, line, fn existing ->
              if existing < line do
                existing
              else
                line
              end
            end)
          end)

        scope_path = get_scope_path(opts, scopes_to_lines, nil, opt_path)

        case get_constructors(extensions, scope_path, hint, type) do
          [] ->
            :ignore

          constructors ->
            suggestions =
              case find_option(constructors, type) do
                {key, config} ->
                  option_values(key, config, hint, opts)

                _ ->
                  # Check for an edge case where we are editing the first argument of a constructor
                  with {:value, option} <- type,
                       entity when not is_nil(entity) <-
                         find_building_entity(constructors, option),
                       [arg | _] <- entity.args,
                       config when not is_nil(config) <- entity.schema[arg] do
                    option_values(arg, config, hint, opts)
                  else
                    _ ->
                      Enum.map(constructors, fn
                        {key, config} ->
                          option_suggestions(key, config, type)

                        %{__struct__: Ash.Dsl.Entity} = entity ->
                          entity_suggestions(entity)

                        %{__struct__: Ash.Dsl.Section} = section ->
                          section_suggestions(section)
                      end)
                  end
              end

            {:override, List.flatten(suggestions)}
        end
      else
        _ ->
          :ignore
      end
    end

    defp find_building_entity(constructors, option) do
      Enum.find(constructors, fn
        %{__struct__: Ash.Dsl.Entity, name: ^option} ->
          true

        _ ->
          false
      end)
    end

    defp find_option(constructors, {:value, option}) do
      Enum.find_value(constructors, fn
        {^option, _} = opt ->
          opt

        %{__struct__: Ash.Dsl.Entity, name: ^option, args: [arg | _], schema: schema} ->
          {arg, schema[arg]}

        _ ->
          false
      end)
    end

    defp find_option(constructors, {:arg, arg_index}) do
      Enum.find(constructors, fn
        {_option, config} ->
          config[:arg_index] == arg_index

        _ ->
          false
      end)
    end

    defp find_option(_, _), do: nil

    defp section_suggestions(section) do
      snippet = Map.get(section, :snippet)

      snippet =
        if snippet && snippet != "" do
          snippet
        else
          "$0"
        end

      %{
        type: :generic,
        kind: :function,
        label: to_string(section.name),
        snippet: "#{section.name} do\n  #{snippet}\nend",
        detail: "Dsl Section",
        documentation: Map.get(section, :docs) || ""
      }
    end

    defp entity_suggestions(entity) do
      snippet = Map.get(entity, :snippet)

      snippet =
        if snippet && snippet != "" do
          snippet
        else
          "$0"
        end

      %{
        type: :generic,
        kind: :function,
        label: to_string(entity.name),
        snippet: "#{entity.name} #{args(entity)}do\n  #{snippet}\nend",
        detail: "Dsl Entity",
        documentation: Map.get(entity, :docs) || ""
      }
    end

    defp option_suggestions(key, config, type) do
      snippet =
        if config[:snippet] && config[:snippet] != "" do
          config[:snippet]
        else
          default_snippet(config)
        end

      snippet =
        case type do
          :option ->
            "#{key}: #{snippet}"

          {:arg, _} ->
            "#{key}: #{snippet}"

          _ ->
            "#{key} #{snippet}"
        end

      %{
        type: :generic,
        kind: :function,
        label: to_string(key),
        snippet: snippet,
        detail: "Option",
        documentation: config[:doc]
      }
    end

    defp get_option(text) when is_binary(text) do
      case Regex.named_captures(~r/\s(?<option>[^\s]+):[[:blank:]]+$/, text) do
        %{"option" => option} when option != "" ->
          try do
            String.to_existing_atom(option)
          rescue
            _ ->
              nil
          end

        _ ->
          nil
      end
    end

    defp get_option(_), do: nil

    defp get_section_option(text) when is_binary(text) do
      case Regex.named_captures(~r/\n[[:blank:]]+(?<option>[^\s]+)[[:blank:]]+$/, text) do
        %{"option" => option} when option != "" ->
          try do
            String.to_existing_atom(option)
          rescue
            _ ->
              nil
          end

        _ ->
          nil
      end
    end

    defp get_section_option(_), do: nil

    defp option_values(key, config, hint, opts) do
      case config[:type] do
        :boolean ->
          enum_suggestion([true, false], hint, "boolean")

        {:one_of, values} ->
          enum_suggestion(values, hint, to_string(key))

        {:in, values} ->
          enum_suggestion(values, hint, to_string(key))

        :ash_type ->
          builtin_types = Types.find_builtin_types(hint, opts.cursor_context)
          custom_types = Types.find_custom_types(hint, opts.module_store)

          builtin_types ++ custom_types

        :ash_resource ->
          Resource.find_resources(hint)

        {:ash_behaviour, behaviour, builtins} ->
          Resource.find_ash_behaviour_impls(behaviour, builtins, hint, opts.module_store)

        {:behaviour, behaviour} ->
          Resource.find_ash_behaviour_impls(behaviour, nil, hint, opts.module_store)

        {:ash_behaviour, behaviour} ->
          Resource.find_ash_behaviour_impls(behaviour, nil, hint, opts.module_store)

        _ ->
          []
      end
    end

    defp enum_suggestion(values, hint, label) do
      Enum.flat_map(values, fn v ->
        inspected = inspect(v)

        if Matcher.match?(inspected, hint) do
          [
            %{
              type: :generic,
              kind: :type_parameter,
              label: label,
              insert_text: inspect(v),
              snippet: inspect(v),
              documentation: inspect(v),
              priority: 0
            }
          ]
        else
          []
        end
      end)
    end

    defp default_snippet(config) do
      cond do
        config[:type] == :boolean && config[:default] in [true, false] ->
          "#{to_string(!config[:default])}"

        config[:type] == :string ->
          "\"$0\""

        match?({:list, {:in, _list}}, config[:type]) ->
          if config[:default] do
            inspect(config[:default])
          else
            "$0"
          end

        match?({:list, _}, config[:type]) ->
          "[$0]"

        match?({:in, _}, config[:type]) ->
          if config[:default] do
            inspect(config[:default])
          else
            {:in, list} = config[:type]
            inspect(Enum.at(list, 0))
          end

        match?({:one_of, _}, config[:type]) ->
          if config[:default] do
            inspect(config[:default])
          else
            {:one_of, list} = config[:type]
            inspect(Enum.at(list, 0))
          end

        config[:type] == :atom ->
          ":$0"

        config[:type] == :mfa ->
          "{${1:module}, :${2:function}, [${3:args}]}"

        config[:type] == :keyword_list ->
          "[$0]"

        true ->
          "$0"
      end
    end

    defp args(entity) do
      case entity.args || [] do
        [] ->
          ""

        args ->
          args
          |> Enum.with_index()
          |> Enum.map(fn {arg, index} ->
            "${#{index + 1}:#{arg}}"
          end)
          |> Enum.intersperse(", ")
          |> Enum.join()
          |> Kernel.<>(" ")
      end
    end

    defp get_constructors(extensions, [], hint, _type) do
      Enum.flat_map(
        extensions,
        fn extension ->
          try do
            Enum.filter(extension.sections(), fn section ->
              Matcher.match?(to_string(section.name), hint)
            end)
          rescue
            _ ->
              []
          end
        end
      )
    end

    defp get_constructors(extensions, [first | rest], hint, type) do
      Enum.flat_map(
        extensions,
        fn extension ->
          try do
            Enum.flat_map(extension.sections(), fn section ->
              if section.name == first do
                do_find_constructors(section, rest, hint, type)
              else
                []
              end
            end)
          rescue
            _ ->
              []
          end
        end
      )
    end

    defp do_find_constructors(entity_or_section, path, hint, type, recursives \\ [])

    defp do_find_constructors(%{__struct__: Ash.Dsl.Entity} = entity, [], hint, type, recursives) do
      case type do
        {:value, _value} ->
          entity.schema ++
            Enum.flat_map(entity.entities || [], &elem(&1, 1)) ++
            Enum.uniq(recursives ++ List.wrap(recursive_for(entity)))

        {:arg, arg_index} ->
          if arg_index >= Enum.count(entity.args || []) do
            find_opt_hints(entity, hint) ++
              find_entity_hints(entity, hint, recursives)
          else
            Enum.map(entity.schema, fn {key, value} ->
              arg_index = Enum.find_index(entity.args || [], &(&1 == key))

              if arg_index do
                {key, Keyword.put(value, :arg_index, arg_index)}
              else
                {key, value}
              end
            end) ++
              Enum.uniq(
                recursives ++
                  List.wrap(recursive_for(entity))
              )
          end

        _ ->
          find_opt_hints(entity, hint) ++
            find_entity_hints(entity, hint, recursives)
      end
    end

    defp do_find_constructors(section, [], hint, _type, recursives) do
      find_opt_hints(section, hint) ++
        find_entity_hints(section, hint, []) ++
        Enum.filter(section.sections, fn section ->
          Matcher.match?(to_string(section.name), hint)
        end) ++ recursives
    end

    defp do_find_constructors(
           %{__struct__: Ash.Dsl.Entity} = entity,
           [next | rest],
           hint,
           type,
           recursives
         ) do
      entity.entities
      |> Kernel.||([])
      |> Enum.flat_map(&elem(&1, 1))
      |> Enum.concat(recursives)
      |> Enum.concat(List.wrap(recursive_for(entity)))
      |> Enum.filter(&(&1.name == next))
      |> Enum.flat_map(
        &do_find_constructors(
          &1,
          rest,
          hint,
          type,
          Enum.uniq(recursives ++ List.wrap(recursive_for(entity)))
        )
      )
      |> Enum.uniq()
    end

    defp do_find_constructors(section, [first | rest], hint, type, recursives) do
      Enum.flat_map(section.entities, fn entity ->
        if entity.name == first do
          do_find_constructors(entity, rest, hint, type)
        else
          []
        end
      end) ++
        Enum.flat_map(section.sections, fn section ->
          if section.name == first do
            do_find_constructors(section, rest, hint, type)
          else
            []
          end
        end) ++ recursives
    end

    defp recursive_for(entity) do
      if entity.recursive_as do
        entity
      end
    end

    defp find_opt_hints(%{__struct__: Ash.Dsl.Entity} = entity, hint) do
      Enum.flat_map(entity.schema, fn {key, value} ->
        if Matcher.match?(to_string(key), hint) do
          arg_index = Enum.find_index(entity.args || [], &(&1 == key))

          if arg_index do
            [{key, Keyword.put(value, :arg_index, arg_index)}]
          else
            [{key, value}]
          end
        else
          []
        end
      end)
    end

    defp find_opt_hints(section, hint) do
      Enum.filter(section.schema, fn {key, _value} ->
        Matcher.match?(to_string(key), hint)
      end)
    end

    defp find_entity_hints(%{__struct__: Ash.Dsl.Entity} = entity, hint, recursives) do
      entity.entities
      |> Keyword.values()
      |> Enum.concat(recursives)
      |> Enum.concat(List.wrap(recursive_for(entity)))
      |> List.flatten()
      |> Enum.filter(fn entity ->
        Matcher.match?(to_string(entity.name), hint)
      end)
    end

    defp find_entity_hints(section, hint, _recursives) do
      Enum.filter(section.entities, fn entity ->
        Matcher.match?(to_string(entity.name), hint)
      end)
    end

    defp get_scope_path(opts, scopes_to_lines, env, path) do
      env = env || opts.env

      with earliest_line when not is_nil(earliest_line) <-
             scopes_to_lines[env.scope_id],
           [%{func: func}] when func != :defmodule <-
             opts.buffer_metadata.calls[earliest_line],
           next_env when not is_nil(next_env) <-
             opts.buffer_metadata.lines_to_env[earliest_line] do
        get_scope_path(opts, scopes_to_lines, next_env, [func | path])
      else
        _ ->
          path
      end
    end

    defp ash_extension?(module) do
      true in List.wrap(module.module_info()[:attributes][:ash_dsl])
    rescue
      _ -> false
    end

    defp default_extensions(dsl_mod) do
      dsl_mod.module_info[:attributes][:ash_default_extensions]
      |> List.wrap()
      |> List.flatten()
    end

    defp parse_extensions(opts, extension_kinds) do
      Enum.flat_map([:extensions | extension_kinds], fn extension_kind ->
        case Regex.named_captures(
               ~r/#{extension_kind}:\s+?(?<extensions>(\[[^\]]*\])|([^\s]*))?[\,\s$]/,
               opts.cursor_context.text_before
             ) do
          %{"extensions" => extensions} when extensions != "" ->
            extensions
            |> String.replace("[", "")
            |> String.replace("]", "")
            |> String.split(",")
            |> Enum.map(&String.trim/1)

          _ ->
            []
        end
      end)
      |> Enum.uniq()
      |> Enum.map(&Module.concat([&1]))
      |> Enum.filter(&Code.ensure_loaded?/1)
    end
  end
end
