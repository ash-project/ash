# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

if Code.ensure_loaded?(Igniter) do
  defmodule Spark.Igniter do
    @moduledoc """
    Helpers for patching Spark DSLs.
    """
    require Igniter.Code.Common
    alias Sourceror.Zipper

    @doc "Prepends a new section or list of sections to the section order in a formatter configuration."
    def prepend_to_section_order(igniter, type, sections) do
      sections = List.wrap(sections)

      Igniter.Project.Config.configure(
        igniter,
        "config.exs",
        :spark,
        [:formatter, type, :section_order],
        sections,
        updater: fn zipper ->
          sections
          |> Enum.reverse()
          |> Enum.reduce({:ok, zipper}, fn section, {:ok, zipper} ->
            case Igniter.Code.List.prepend_new_to_list(zipper, section) do
              {:ok, zipper} -> {:ok, zipper}
              :error -> {:ok, zipper}
            end
          end)
        end
      )
    end

    @doc """
    Gets an option at a given path within a DSL.
    We will attempt to expand literals using the environment at the path
    but this is only guaranteed to return the *AST* at that option, *not* necessarily a value.

    Additionally, this only finds options set explicitly in the body of the resource, not by an extension.
    """
    @spec get_option(Igniter.t(), module(), list(atom)) ::
            {Igniter.t(), {:ok, Macro.t()} | :error}
    def get_option(igniter, module, path) do
      {body, [last]} = Enum.split(List.wrap(path), -1)
      path = Enum.map(body, &{:section, &1}) ++ [{:option, last}]

      find(igniter, module, fn _search_module, zipper ->
        do_get_option(zipper, path)
      end)
      |> case do
        {:ok, igniter, _module, value} -> {igniter, {:ok, value}}
        {:error, igniter} -> {igniter, :error}
      end
    end

    defp do_get_option(zipper, [{:option, name}]) do
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, 1),
           {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 0) do
        zipper = Igniter.Code.Common.maybe_move_to_single_child_block(zipper)

        case Igniter.Code.Common.expand_literal(zipper) do
          {:ok, value} -> {:ok, value}
          :error -> {:ok, zipper.node}
        end
      else
        _ -> :error
      end
    end

    defp do_get_option(zipper, [{:section, name} | rest]) do
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, 1),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        do_get_option(zipper, rest)
      else
        _ -> :error
      end
    end

    @doc "Searches for a match to a zipper function inside a DSL and all of its fragments."
    @spec find(Igniter.t(), module, (module(), Zipper.t() -> {:ok, value} | :error)) ::
            {:ok, Igniter.t(), module(), value} | {:error, Igniter.t()}
          when value: term()
    def find(igniter, module, callback) do
      {:ok, {igniter, _source, zipper}} = Igniter.Project.Module.find_module(igniter, module)

      zipper =
        case Igniter.Code.Common.move_to_do_block(zipper) do
          {:ok, zipper} -> zipper
          _ -> zipper
        end

      zipper
      |> search_modules(module)
      |> Enum.reduce_while({igniter, :error}, fn search_module, {igniter, :error} ->
        {igniter, zipper} =
          if search_module == module do
            {igniter, {:ok, zipper}}
          else
            with {:ok, {igniter, _source, zipper}} <-
                   Igniter.Project.Module.find_module(igniter, search_module),
                 {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
              {igniter, {:ok, zipper}}
            else
              {:error, igniter} ->
                {igniter, :error}

              _ ->
                {igniter, :error}
            end
          end

        with {:ok, zipper} <- zipper,
             {:ok, value} <- callback.(search_module, zipper) do
          {:halt, {:ok, igniter, search_module, value}}
        else
          _ -> {:cont, {igniter, :error}}
        end
      end)
      |> case do
        {igniter, :error} -> {:error, igniter}
        {:ok, igniter, search_module, value} -> {:ok, igniter, search_module, value}
      end
    end

    @doc "Sets an option at a given path within in a DSL."
    @spec set_option(
            Igniter.t(),
            module,
            dsl_path :: list(atom),
            value :: term(),
            (Zipper.t() -> {:ok, Zipper.t()} | {:error, term | list(term)} | :error)
          ) :: Igniter.t()
    def set_option(igniter, module, path, value, updater \\ &{:ok, &1}) do
      {body, [last]} = Enum.split(List.wrap(path), -1)
      path = Enum.map(body, &{:section, &1}) ++ [{:option, last}]

      update_dsl(igniter, module, path, value, updater)
    end

    def update_dsl(igniter, module, path, value, func) do
      Igniter.Project.Module.find_and_update_module!(igniter, module, fn zipper ->
        do_update_dsl(zipper, path, value, func)
      end)
    end

    defp do_update_dsl(zipper, [], value, fun) do
      if Igniter.Code.Common.node_matches_pattern?(zipper, v when is_nil(v)) do
        {:ok, Sourceror.Zipper.replace(zipper, value)}
      else
        fun.(zipper)
      end
    end

    defp do_update_dsl(zipper, [{:section, name} | rest], value, fun) do
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, 1),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        do_update_dsl(zipper, rest, value, fun)
      else
        _ ->
          zipper =
            Igniter.Code.Common.add_code(zipper, """
            #{name} do
            end
            """)

          with {:ok, zipper} <-
                 Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, 1),
               {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
            do_update_dsl(zipper, rest, value, fun)
          else
            _ ->
              {:ok, zipper}
          end
      end
    end

    defp do_update_dsl(zipper, [{:option, name}], value, fun) do
      case Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, 1) do
        {:ok, zipper} ->
          Igniter.Code.Function.update_nth_argument(zipper, 0, fun)

        _ ->
          added_zipper = Igniter.Code.Common.add_code(zipper, {name, [], [value]})

          case Igniter.Code.Function.move_to_function_call_in_current_scope(added_zipper, name, 1) do
            {:ok, zipper} ->
              {:ok, zipper}

            _ ->
              {:ok, zipper}
          end
      end
    end

    defp do_update_dsl(_zipper, [{:option, name} | _], _value, _fun) do
      raise ArgumentError,
            "{:option, #{inspect(name)}} was found as a non-leaf node in a path to update. Options must be the last item in the list."
    end

    @doc "Removes an extension from a DSL module."
    @spec remove_extension(Igniter.t(), module(), module(), atom(), module(), boolean()) ::
            Igniter.t()
    def remove_extension(igniter, module, type, key, extension, singleton? \\ false) do
      Igniter.Project.Module.find_and_update_module!(igniter, module, fn zipper ->
        with {:ok, zipper} <- Igniter.Code.Module.move_to_use(zipper, type) do
          if Igniter.Code.Common.node_matches_pattern?(zipper, {_, _, [_]}) do
            :error
          else
            if singleton? do
              case Igniter.Code.Keyword.remove_keyword_key(zipper, key) do
                {:ok, zipper} ->
                  {:ok, zipper}

                _ ->
                  :error
              end
            else
              Igniter.Code.Keyword.put_in_keyword(zipper, [key], [], fn zipper ->
                Igniter.Code.List.remove_from_list(zipper, fn nested_zipper ->
                  Igniter.Code.Common.nodes_equal?(nested_zipper, extension)
                end)
              end)
            end
          end
        end
        |> case do
          :error ->
            {:ok, zipper}

          {:ok, zipper} ->
            do_remove_constructors(zipper, type, extension)
        end
      end)
    end

    defp do_remove_constructors(zipper, type, extension) do
      constructors_to_remove = extension_constructors(extension)

      zipper = Zipper.topmost(zipper)
      {:ok, zipper} = Igniter.Code.Module.move_to_module_using(zipper, type)

      remove_constructors(zipper, constructors_to_remove)
    end

    @doc "Returns `{igniter, true}` if the module has the extension, or `{igniter, false}` otherwise."
    @spec has_extension(Igniter.t(), module(), module(), atom(), module()) ::
            {Igniter.t(), boolean()}
    def has_extension(igniter, module, type, key, extension) do
      case Igniter.Project.Module.find_module(igniter, module) do
        {:ok, {igniter, _source, zipper}} ->
          with {:ok, zipper} <- Igniter.Code.Module.move_to_use(zipper, type),
               {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
               {:ok, zipper} <- Igniter.Code.Keyword.get_key(zipper, key) do
            match? =
              if Igniter.Code.List.list?(zipper) do
                match?(
                  {:ok, _},
                  Igniter.Code.List.move_to_list_item(
                    zipper,
                    &Igniter.Code.Common.nodes_equal?(&1, extension)
                  )
                )
              else
                Igniter.Code.Common.nodes_equal?(zipper, extension)
              end

            {igniter, match?}
          else
            _ ->
              {igniter, false}
          end

        {:error, igniter} ->
          {igniter, false}
      end
    end

    # sobelow_skip ["DOS.StringToAtom"]
    @doc "Adds an extension to a DSL module."
    @spec add_extension(Igniter.t(), module(), module(), atom(), module(), boolean()) ::
            Igniter.t()
    def add_extension(igniter, module, type, key, extension, singleton? \\ false) do
      extension_mod = extension
      extension = {:__aliases__, [], Enum.map(Module.split(extension), &String.to_atom/1)}

      Igniter.Project.Module.find_and_update_module!(igniter, module, fn zipper ->
        case Igniter.Code.Module.move_to_use(zipper, type) do
          {:ok, zipper} ->
            if Igniter.Code.Common.node_matches_pattern?(zipper, {_, _, [_]}) do
              value =
                if singleton? do
                  extension
                else
                  [extension]
                end

              Igniter.Code.Function.append_argument(zipper, [{key, value}])
            else
              remove_extension =
                if singleton? do
                  with {:ok, arg_zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
                       {:ok, value_zipper} <- Igniter.Code.Keyword.get_key(arg_zipper, key),
                       false <- Igniter.Code.Common.nodes_equal?(value_zipper, extension_mod),
                       module_zipper <- Igniter.Code.Common.expand_aliases(value_zipper),
                       {:__aliases__, _, parts} <- Zipper.node(module_zipper) do
                    {:ok, Module.concat(parts)}
                  else
                    _ ->
                      :error
                  end
                else
                  :error
                end

              Igniter.Code.Function.update_nth_argument(zipper, 1, fn zipper ->
                if singleton? do
                  case Igniter.Code.Keyword.put_in_keyword(zipper, [key], extension, fn x ->
                         {:ok, Igniter.Code.Common.replace_code(x, extension)}
                       end) do
                    {:ok, zipper} ->
                      {:ok, zipper}

                    _ ->
                      {:ok, zipper}
                  end
                else
                  Igniter.Code.Keyword.put_in_keyword(zipper, [key], [extension], fn zipper ->
                    Igniter.Code.List.prepend_new_to_list(
                      zipper,
                      extension
                    )
                  end)
                end
              end)
              |> case do
                {:ok, zipper} ->
                  case remove_extension do
                    {:ok, module} ->
                      if Code.ensure_loaded?(module) &&
                           Spark.implements_behaviour?(module, Spark.Dsl.Extension) do
                        do_remove_constructors(zipper, type, module)
                      else
                        {:ok, zipper}
                      end

                    :error ->
                      {:ok, zipper}
                  end

                :error ->
                  :error
              end
            end

          _ ->
            {:ok, zipper}
        end
      end)
    end

    defp remove_constructors(zipper, []), do: {:ok, zipper}

    defp remove_constructors(zipper, [{name, arity} | rest] = all) do
      Igniter.Code.Common.within(zipper, fn zipper ->
        case Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, name, arity) do
          {:ok, zipper} ->
            {:ok, Zipper.remove(zipper)}

          :error ->
            :error
        end
      end)
      |> case do
        :error ->
          remove_constructors(zipper, rest)

        {:ok, zipper} ->
          remove_constructors(zipper, all)
      end
    end

    defp extension_constructors(module) do
      module.sections()
      |> Enum.flat_map(fn section ->
        if section.top_level? do
          nested_entities =
            Enum.flat_map(section.entities, fn entity ->
              total_count = Enum.count(entity.args)
              optional_count = Enum.count(entity.args, &(not is_atom(&1)))

              required_count = total_count - optional_count

              Enum.map(required_count..total_count, fn i ->
                {entity.name, i}
              end)
            end)

          nested_sections =
            Enum.map(section.sections, fn section ->
              {section.name, 1}
            end)

          nested_entities ++ nested_sections
        else
          [{section.name, 1}]
        end
      end)
    end

    # sobelow_skip ["RCE.CodeModule"]
    defp search_modules(zipper, base) do
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :use,
               2,
               fn zipper ->
                 with {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
                      {:ok, _} <- Igniter.Code.Keyword.get_key(zipper, :fragments) do
                   true
                 else
                   _ ->
                     false
                 end
               end
             ),
           {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
           {:ok, zipper} <- Igniter.Code.Keyword.get_key(zipper, :fragments) do
        evaled =
          try do
            case Igniter.Code.Common.expand_literal(zipper) do
              {:ok, value} -> value
              :error -> Code.eval_quoted(zipper.node)
            end
          rescue
            _e ->
              []
          end

        if is_list(evaled) do
          Enum.uniq([base | Enum.filter(evaled, &is_atom/1)])
        else
          [base]
        end
      else
        _ ->
          [base]
      end
    end
  end
end
