# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.TypeInference do
  @moduledoc false

  @doc false
  def capture_definitions(module) do
    module
    |> Module.definitions_in()
    |> Enum.reject(fn {name, _arity} -> name == :__ash_type_inference_definitions__ end)
    |> Enum.flat_map(fn definition ->
      case Module.get_definition(module, definition) do
        {:v1, visibility, metadata, clauses} when visibility in [:def, :defp] ->
          [{definition, visibility, metadata, clauses}]

        _ ->
          []
      end
    end)
  end

  @doc false
  def witness_entries(resources) do
    Enum.flat_map(resources, fn resource ->
      changes =
        for change_module <- dispatched_change_modules(resource),
            definition = specialized_change_witness_definition(resource, change_module),
            definition != nil do
          witness_entry({resource, :change, change_module}, definition)
        end

      inline_changes = inline_change_witness_entries(resource)

      validations =
        for validation_module <- dispatched_validation_modules(resource),
            definition = specialized_validation_witness_definition(resource, validation_module),
            definition != nil do
          witness_entry({resource, :validation, validation_module}, definition)
        end

      preparations =
        for preparation_module <- dispatched_preparation_modules(resource),
            definition = specialized_preparation_witness_definition(resource, preparation_module),
            definition != nil do
          witness_entry({resource, :preparation, preparation_module}, definition)
        end

      changes ++ inline_changes ++ validations ++ preparations
    end)
  end

  def transform_lifted_function(function, :run, _caller, %{
        entity_name: :action,
        entity_body: entity_body
      }) do
    argument_names = argument_names(entity_body)

    {function, transformed?} =
      Macro.prewalk(function, false, fn
        {:def, meta, [head, [do: body]]} = definition, transformed? ->
          case first_argument(head) do
            {name, _, context} = input when is_atom(name) and is_atom(context) ->
              if variable_used?(body, input) do
                arguments = exact_map(input, :arguments, argument_names)

                transformed =
                  quote generated: false do
                    def unquote(head) do
                      unquote(input) = %{
                        unquote(input)
                        | arguments: unquote(arguments)
                      }

                      unquote(body)
                    end
                  end
                  |> put_line(meta[:line])

                {transformed, true}
              else
                {definition, transformed?}
              end

            _ ->
              {definition, transformed?}
          end

        other, transformed? ->
          {other, transformed?}
      end)

    if transformed?, do: mark_user_code(function), else: function
  end

  def transform_lifted_function(function, _key, _caller, _context), do: function

  def change_dispatcher_definition(_resource) do
    quote generated: false do
      def __ash_dispatch_change__(_action_name, module, changeset, opts, context) do
        module.change(changeset, opts, context)
      end
    end
  end

  @doc false
  def specialized_change_witness_definition(resource, change_module) do
    attributes = Ash.Resource.Info.attributes(resource)

    with {:ok, definitions} <- change_definitions(change_module) do
      clauses =
        resource
        |> Ash.Resource.Info.actions()
        |> Enum.flat_map(fn action ->
          managed_inputs = managed_relationship_inputs(resource, action)

          opts =
            action
            |> then(&Ash.Resource.Info.action_changes(resource, &1))
            |> Enum.flat_map(fn
              %{change: {^change_module, opts}} -> [opts]
              _ -> []
            end)

          if opts == [] do
            []
          else
            arguments =
              typed_map(quote(do: raw_changeset), :arguments, action.arguments, managed_inputs)

            action_attributes = typed_map(quote(do: raw_changeset), :attributes, attributes)

            [local_witness_clause(action.name, arguments, action_attributes, opts, definitions)]
          end
        end)

      if clauses == [] do
        nil
      else
        witness_module_definition(change_module, resource, nil, definitions, clauses)
      end
    else
      _ -> nil
    end
  end

  @doc false
  def dispatched_change_modules(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.flat_map(&Ash.Resource.Info.action_changes(resource, &1))
    |> Enum.flat_map(fn
      %{change: {module, _opts}} when is_atom(module) -> [module]
      _ -> []
    end)
    |> Enum.reject(&(&1 == Ash.Resource.Change.ManageRelationship))
    |> Enum.reject(&(&1 == Ash.Resource.Change.Function))
    |> Enum.filter(fn module ->
      match?({:ok, _}, change_definitions(module))
    end)
    |> Enum.uniq()
  end

  defp inline_change_witness_entries(resource) do
    attributes = Ash.Resource.Info.attributes(resource)

    actions =
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.flat_map(fn action ->
        callbacks =
          resource
          |> Ash.Resource.Info.action_changes(action)
          |> Enum.flat_map(fn
            %{change: {Ash.Resource.Change.Function, opts}} ->
              inline_callback(opts[:fun], resource)

            _ ->
              []
          end)

        if callbacks == [], do: [], else: [{action, callbacks}]
      end)

    roots =
      actions
      |> Enum.flat_map(&elem(&1, 1))
      |> Enum.map(fn {name, extra_arguments} -> {name, 2 + length(extra_arguments)} end)
      |> Enum.uniq()

    with [_ | _] <- roots,
         {:ok, definitions} <- beam_definitions(resource),
         [_ | _] = definitions <- definition_closure(definitions, roots) do
      clauses =
        Enum.map(actions, fn {action, callbacks} ->
          arguments = typed_map(quote(do: raw_changeset), :arguments, action.arguments)
          action_attributes = typed_map(quote(do: raw_changeset), :attributes, attributes)

          calls =
            Enum.map(callbacks, fn {name, extra_arguments} ->
              quote do
                unquote(name)(changeset, context, unquote_splicing(extra_arguments))
              end
            end)

          quote generated: false do
            def __ash_type_witness__(
                  unquote(action.name),
                  %Ash.Changeset{} = raw_changeset,
                  context
                ) do
              changeset = %Ash.Changeset{
                raw_changeset
                | arguments: unquote(arguments),
                  attributes: unquote(action_attributes)
              }

              unquote_splicing(calls)
              changeset
            end
          end
        end)

      definition =
        witness_module_definition(resource, resource, InlineChange, definitions, clauses)

      [
        witness_entry({resource, :inline_change}, definition,
          diagnostic_functions: inline_diagnostic_functions(definitions, roots),
          generated_functions: roots
        )
      ]
    else
      _ -> []
    end
  end

  defp inline_callback(fun, resource) when is_function(fun) do
    info = Function.info(fun)

    case {info[:module], info[:name], info[:arity], info[:env]} do
      {^resource, name, arity, []} when is_atom(name) and arity >= 2 ->
        [{name, []}]

      _ ->
        []
    end
  end

  defp inline_callback({module, name, extra_arguments}, resource)
       when module == resource and is_atom(name) and is_list(extra_arguments) do
    [{name, Enum.map(extra_arguments, &Macro.escape/1)}]
  end

  defp inline_callback(_, _resource), do: []

  defp inline_diagnostic_functions(definitions, roots) do
    functions =
      roots
      |> Enum.flat_map(fn root ->
        case Enum.find(definitions, fn {key, _visibility, _metadata, _clauses} -> key == root end) do
          {_key, _visibility, _metadata, clauses} -> direct_local_captures(clauses)
          nil -> []
        end
      end)
      |> Enum.uniq()

    if functions == [], do: roots, else: functions
  end

  defp direct_local_captures(clauses) do
    Enum.flat_map(clauses, fn {_metadata, _patterns, _guards, body} ->
      direct_local_call(body)
    end)
  end

  defp direct_local_call({:__block__, _metadata, [body]}), do: direct_local_call(body)

  defp direct_local_call(
         {{:., _, [{:&, _, [{:/, _, [{name, _, context}, arity]}]}]}, _metadata, _arguments}
       )
       when is_atom(name) and is_atom(context) and is_integer(arity),
       do: [{name, arity}]

  defp direct_local_call({:&, _, [{:/, _, [{name, _, context}, arity]}]})
       when is_atom(name) and is_atom(context) and is_integer(arity),
       do: [{name, arity}]

  defp direct_local_call({name, _metadata, arguments})
       when is_atom(name) and is_list(arguments),
       do: [{name, length(arguments)}]

  defp direct_local_call(_body), do: []

  defp definition_closure(definitions, roots) do
    definitions_by_name =
      Map.new(definitions, fn {{name, arity}, _, _, _} = definition ->
        {{name, arity}, definition}
      end)

    do_definition_closure(definitions_by_name, MapSet.new(roots), MapSet.new())
  end

  defp do_definition_closure(definitions, pending, found) do
    case Enum.at(pending, 0) do
      nil ->
        found
        |> Enum.flat_map(fn key -> Map.get(definitions, key, []) |> List.wrap() end)

      key ->
        pending = MapSet.delete(pending, key)

        case Map.fetch(definitions, key) do
          {:ok, definition} ->
            dependencies = local_definition_calls(definition, definitions)

            do_definition_closure(
              definitions,
              MapSet.union(pending, MapSet.difference(dependencies, found)),
              MapSet.put(found, key)
            )

          :error ->
            do_definition_closure(definitions, pending, found)
        end
    end
  end

  defp local_definition_calls(definition, definitions) do
    {_key, _visibility, _metadata, clauses} = definition

    Enum.reduce(clauses, MapSet.new(), fn {_metadata, patterns, guards, body}, calls ->
      [patterns, guards, body]
      |> Macro.prewalk(calls, fn
        {:&, _, [{:/, _, [{name, _, context}, arity]}]} = node, calls
        when is_atom(name) and is_atom(context) and is_integer(arity) ->
          key = {name, arity}
          {node, if(Map.has_key?(definitions, key), do: MapSet.put(calls, key), else: calls)}

        {name, _, arguments} = node, calls when is_atom(name) and is_list(arguments) ->
          key = {name, length(arguments)}
          {node, if(Map.has_key?(definitions, key), do: MapSet.put(calls, key), else: calls)}

        node, calls ->
          {node, calls}
      end)
      |> elem(1)
    end)
  end

  @doc false
  def specialized_validation_witness_definition(resource, validation_module) do
    attributes = Ash.Resource.Info.attributes(resource)

    with {:ok, definitions} <- callback_definitions(validation_module) do
      clauses =
        resource
        |> Ash.Resource.Info.actions()
        |> Enum.flat_map(fn action ->
          opts =
            validation_occurrences(resource, action)
            |> Enum.flat_map(fn
              %{module: ^validation_module, opts: opts} -> [opts]
              _ -> []
            end)

          if opts == [],
            do: [],
            else: [validation_witness_clause(action, attributes, opts, definitions)]
        end)

      if clauses == [] do
        nil
      else
        witness_module_definition(
          validation_module,
          resource,
          Validation,
          definitions,
          clauses
        )
      end
    else
      _ -> nil
    end
  end

  @doc false
  def specialized_preparation_witness_definition(resource, preparation_module) do
    with {:ok, definitions} <- callback_definitions(preparation_module) do
      clauses =
        resource
        |> Ash.Resource.Info.actions()
        |> Enum.flat_map(fn action ->
          opts =
            preparation_occurrences(resource, action)
            |> Enum.flat_map(fn
              %{preparation: {^preparation_module, opts}} -> [opts]
              _ -> []
            end)

          if opts == [], do: [], else: [preparation_witness_clause(action, opts, definitions)]
        end)

      if clauses == [] do
        nil
      else
        witness_module_definition(
          preparation_module,
          resource,
          Preparation,
          definitions,
          clauses
        )
      end
    else
      _ -> nil
    end
  end

  @doc false
  def dispatched_validation_modules(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.flat_map(&validation_occurrences(resource, &1))
    |> Enum.flat_map(fn
      %{module: module} when is_atom(module) -> [module]
      _ -> []
    end)
    |> Enum.reject(&(&1 == Ash.Resource.Validation.Function))
    |> Enum.filter(&match?({:ok, _}, callback_definitions(&1)))
    |> Enum.uniq()
  end

  @doc false
  def dispatched_preparation_modules(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.flat_map(&preparation_occurrences(resource, &1))
    |> Enum.flat_map(fn
      %{preparation: {module, _opts}} when is_atom(module) -> [module]
      _ -> []
    end)
    |> Enum.reject(&(&1 == Ash.Resource.Preparation.Function))
    |> Enum.filter(&match?({:ok, _}, callback_definitions(&1)))
    |> Enum.uniq()
  end

  defp local_witness_clause(action_name, arguments, attributes, all_opts, definitions) do
    calls =
      Enum.flat_map(all_opts, fn opts ->
        [
          {:change, [quote(do: changeset), Macro.escape(opts), quote(do: context)]},
          {:atomic, [quote(do: changeset), Macro.escape(opts), quote(do: context)]},
          {:batch_change, [[quote(do: changeset)], Macro.escape(opts), quote(do: context)]}
        ]
        |> Enum.flat_map(fn {name, callback_arguments} ->
          case inline_callback_invocation(definitions, name, callback_arguments) do
            nil -> []
            invocation -> [invocation]
          end
        end)
      end)

    quote generated: false do
      def __ash_type_witness__(
            unquote(action_name),
            %Ash.Changeset{} = raw_changeset,
            _opts,
            context
          ) do
        changeset = %Ash.Changeset{
          raw_changeset
          | arguments: unquote(arguments),
            attributes: unquote(attributes)
        }

        unquote_splicing(calls)
        changeset
      end
    end
  end

  defp inline_callback_invocation(definitions, name, arguments) do
    case Enum.find(definitions, fn {{definition_name, arity}, _, _, _} ->
           definition_name == name and arity == length(arguments)
         end) do
      nil ->
        nil

      {{_name, _arity}, _visibility, _metadata, clauses} ->
        case_clauses =
          Enum.map(clauses, fn {metadata, patterns, guards, body} ->
            tuple_pattern = {:{}, metadata, patterns}

            pattern =
              if guards == [],
                do: tuple_pattern,
                else: {:when, metadata, [tuple_pattern | guards]}

            {:->, metadata, [[pattern], body]}
          end)

        {:case, [generated: false], [{:{}, [], arguments}, [do: case_clauses]]}
    end
  end

  defp validation_witness_clause(action, attributes, all_opts, definitions) do
    {source, source_kind} =
      case action.type do
        type when type in [:create, :update, :destroy] ->
          arguments = typed_map(quote(do: raw_source), :arguments, action.arguments)
          attributes = typed_map(quote(do: raw_source), :attributes, attributes)

          source =
            quote generated: true do
              %Ash.Changeset{
                raw_source
                | arguments: unquote(arguments),
                  attributes: unquote(attributes)
              }
            end

          {source, :changeset}

        :read ->
          arguments = typed_map(quote(do: raw_source), :arguments, action.arguments)

          source =
            quote generated: true do
              %Ash.Query{raw_source | arguments: unquote(arguments)}
            end

          {source, :query}

        :action ->
          arguments = typed_map(quote(do: raw_source), :arguments, action.arguments)

          source =
            quote generated: true do
              %Ash.ActionInput{raw_source | arguments: unquote(arguments)}
            end

          {source, :action_input}
      end

    calls =
      Enum.flat_map(all_opts, fn opts ->
        [:validate, :atomic, :batch_validate]
        |> Enum.flat_map(fn
          :batch_validate when source_kind != :changeset ->
            []

          callback ->
            first_argument =
              if callback == :batch_validate, do: [quote(do: source)], else: quote(do: source)

            case inline_callback_invocation(
                   definitions,
                   callback,
                   [first_argument, Macro.escape(opts), quote(do: context)]
                 ) do
              nil -> []
              invocation -> [invocation]
            end
        end)
      end)

    quote generated: false do
      def __ash_type_witness__(unquote(action.name), raw_source, _opts, context) do
        source = unquote(source)
        unquote_splicing(calls)
        source
      end
    end
  end

  defp preparation_witness_clause(action, all_opts, definitions) do
    arguments = typed_map(quote(do: raw_source), :arguments, action.arguments)

    source =
      case action.type do
        :action ->
          quote generated: true do
            %Ash.ActionInput{raw_source | arguments: unquote(arguments)}
          end

        :read ->
          quote generated: true do
            %Ash.Query{raw_source | arguments: unquote(arguments)}
          end
      end

    quote generated: false do
      def __ash_type_witness__(unquote(action.name), raw_source, _opts, context) do
        source = unquote(source)

        unquote_splicing(
          Enum.flat_map(all_opts, fn opts ->
            case inline_callback_invocation(
                   definitions,
                   :prepare,
                   [quote(do: source), Macro.escape(opts), quote(do: context)]
                 ) do
              nil -> []
              invocation -> [invocation]
            end
          end)
        )

        source
      end
    end
  end

  defp validation_occurrences(resource, action) do
    action_validations =
      (Map.get(action, :changes, []) ++ Map.get(action, :preparations, []))
      |> Enum.filter(&match?(%Ash.Resource.Validation{}, &1))

    global_validations =
      if action.type in [:create, :update, :destroy, :read, :action] do
        Ash.Resource.Info.validations(resource)
        |> Enum.filter(&(action.type in &1.on))
      else
        []
      end

    global_validations ++ action_validations
  end

  defp preparation_occurrences(resource, action) when action.type in [:read, :action] do
    Ash.Resource.Info.preparations(resource, action.type) ++ Map.get(action, :preparations, [])
  end

  defp preparation_occurrences(_resource, _action), do: []

  defp witness_entry(key, definition, metadata \\ []) do
    Map.merge(
      %{
        key: key,
        fingerprint: :crypto.hash(:sha256, :erlang.term_to_binary(definition)),
        definition: definition
      },
      Map.new(metadata)
    )
  end

  defp witness_module_definition(callback_module, resource, namespace, definitions, clauses) do
    witness_module =
      case namespace do
        nil -> Module.concat([Ash.TypeWitness, callback_module, resource])
        namespace -> Module.concat([Ash.TypeWitness, namespace, callback_module, resource])
      end

    local_definitions =
      MapSet.new(definitions, fn {{name, arity}, _visibility, _metadata, _clauses} ->
        {name, arity}
      end)

    definitions =
      definitions
      |> Enum.flat_map(&definition_ast/1)
      |> Enum.map(&rewrite_default_wrapper_super/1)
      |> Enum.map(&rewrite_invoked_local_capture(&1, local_definitions))
      |> Enum.map(&rewrite_self_reference(&1, callback_module, witness_module))
      |> Enum.map(&normalize_compiler_metadata/1)

    quote generated: false do
      defmodule unquote(witness_module) do
        unquote_splicing(definitions)
        unquote_splicing(clauses)
      end
    end
  end

  defp rewrite_invoked_local_capture(definition, local_definitions) do
    Macro.prewalk(definition, fn
      {{:., dot_metadata, [{:&, _, [{:/, _, [{name, _, context}, arity]}]}]}, call_metadata,
       arguments} = node
      when is_atom(name) and is_atom(context) and is_integer(arity) and is_list(arguments) ->
        if length(arguments) == arity and MapSet.member?(local_definitions, {name, arity}) do
          {name, Keyword.merge(dot_metadata, call_metadata), arguments}
        else
          node
        end

      node ->
        node
    end)
  end

  defp change_definitions(module) do
    if function_exported?(module, :__ash_type_inference_definitions__, 0) do
      {:ok, module.__ash_type_inference_definitions__()}
    else
      beam_definitions(module)
    end
  end

  defp callback_definitions(module), do: change_definitions(module)

  defp beam_definitions(module) do
    with path when is_list(path) <- :code.which(module),
         {:ok, {^module, [debug_info: {:debug_info_v1, backend, data}]}} <-
           :beam_lib.chunks(path, [:debug_info]),
         {:ok, %{definitions: definitions}} <- backend.debug_info(:elixir_v1, module, data, []) do
      {:ok, definitions}
    else
      _ -> :error
    end
  end

  defp definition_ast({{name, _arity}, visibility, metadata, clauses})
       when visibility in [:def, :defp] do
    Enum.map(clauses, fn {clause_metadata, arguments, guards, body} ->
      head = {name, metadata, arguments}
      head = if guards == [], do: head, else: {:when, clause_metadata, [head | guards]}
      {visibility, metadata, [head, [do: body]]}
    end)
  end

  defp definition_ast(_definition), do: []

  defp rewrite_self_reference(definition, original_module, witness_module) do
    Macro.prewalk(definition, fn
      ^original_module -> witness_module
      node -> node
    end)
  end

  defp rewrite_default_wrapper_super(definition) do
    Macro.prewalk(definition, fn
      {:super, metadata, arguments} = super_call when is_list(metadata) ->
        if metadata[:default] == true do
          case metadata[:super] do
            {_visibility, name} when is_atom(name) ->
              {name, Keyword.drop(metadata, [:default, :super]), arguments}

            _ ->
              super_call
          end
        else
          super_call
        end

      node ->
        node
    end)
  end

  defp normalize_compiler_metadata(definition) do
    Macro.prewalk(definition, fn
      {:"_&", metadata, arguments} when is_list(metadata) ->
        case Keyword.fetch(metadata, :version) do
          {:ok, version} when is_integer(version) ->
            name = String.to_atom("_ash_capture_#{version}")
            {name, Keyword.delete(metadata, :counter), arguments}

          _ ->
            {:"_&", Keyword.delete(metadata, :counter), arguments}
        end

      {name, metadata, arguments} when is_list(metadata) ->
        {name, Keyword.delete(metadata, :counter), arguments}

      node ->
        node
    end)
  end

  defp typed_map(input, field, fields, nested_inputs \\ %{}) do
    {:%{}, [],
     Enum.map(fields, fn field_spec ->
       name = field_spec.name

       value =
         quote generated: true do
           unquote(input).unquote(field)[unquote(name)]
         end

       narrowed =
         case nested_inputs do
           %{^name => nested_fields} ->
             narrow_nested_input(value, field_spec, nested_fields)

           _ ->
             narrow_type(
               value,
               field_spec.type,
               field_spec.constraints,
               field_spec.allow_nil?
             )
         end

       {name, narrowed}
     end)}
  end

  defp narrow_nested_input(value, %{type: {:array, _}, allow_nil?: allow_nil?}, fields) do
    item = Macro.var(:item, __MODULE__)
    narrowed_item = nested_map(item, fields)

    narrowed =
      quote generated: true do
        case unquote(value) do
          [] -> []
          [unquote(item) | _] -> [unquote(narrowed_item)]
        end
      end

    allow_nil(value, narrowed, allow_nil?)
  end

  defp narrow_nested_input(value, %{allow_nil?: allow_nil?}, fields) do
    allow_nil(value, nested_map(value, fields), allow_nil?)
  end

  defp nested_map(value, fields) do
    {:%{}, [],
     Enum.map(fields, fn field ->
       field_value =
         quote generated: true do
           unquote(value)[unquote(field.name)]
         end

       {field.name, narrow_type(field_value, field.type, field.constraints, true)}
     end)}
  end

  defp exact_map(input, field, names) do
    {:%{}, [],
     Enum.map(names, fn name ->
       value =
         quote generated: true do
           unquote(input).unquote(field)[unquote(name)]
         end

       {name, value}
     end)}
  end

  defp narrow_type(value, {:array, item_type}, constraints, allow_nil?) do
    item_constraints = constraints[:items] || []
    item = Macro.var(:item, __MODULE__)
    narrowed_item = narrow_type(item, item_type, item_constraints, false)

    narrowed =
      quote generated: true do
        case unquote(value) do
          [] -> []
          [unquote(item) | _] -> [unquote(narrowed_item)]
        end
      end

    allow_nil(value, narrowed, allow_nil?)
  end

  defp narrow_type(value, type, constraints, allow_nil?) do
    type = Ash.Type.get_type(type)
    narrowed = narrow_non_nil(value, type, constraints)
    allow_nil(value, narrowed, allow_nil?)
  end

  defp narrow_non_nil(value, type, _constraints)
       when type in [Ash.Type.String, Ash.Type.Binary, Ash.Type.UUID, Ash.Type.UUIDv7] do
    guarded(value, :is_binary)
  end

  defp narrow_non_nil(value, Ash.Type.Integer, _constraints), do: guarded(value, :is_integer)
  defp narrow_non_nil(value, Ash.Type.Float, _constraints), do: guarded(value, :is_float)
  defp narrow_non_nil(value, Ash.Type.Boolean, _constraints), do: guarded(value, :is_boolean)

  defp narrow_non_nil(value, type, _constraints)
       when type in [Ash.Type.Atom, Ash.Type.Module] do
    guarded(value, :is_atom)
  end

  defp narrow_non_nil(value, Ash.Type.Map, constraints) do
    case constraints[:fields] do
      fields when is_list(fields) and fields != [] -> constrained_map(value, fields)
      _ -> guarded(value, :is_map)
    end
  end

  defp narrow_non_nil(value, Ash.Type.Tuple, _constraints), do: guarded(value, :is_tuple)
  defp narrow_non_nil(value, Ash.Type.Decimal, _constraints), do: struct_match(value, Decimal)
  defp narrow_non_nil(value, Ash.Type.Date, _constraints), do: struct_match(value, Date)
  defp narrow_non_nil(value, Ash.Type.Time, _constraints), do: struct_match(value, Time)
  defp narrow_non_nil(value, Ash.Type.TimeUsec, _constraints), do: struct_match(value, Time)

  defp narrow_non_nil(value, type, _constraints)
       when type in [Ash.Type.DateTime, Ash.Type.UtcDatetime, Ash.Type.UtcDatetimeUsec] do
    struct_match(value, DateTime)
  end

  defp narrow_non_nil(value, Ash.Type.NaiveDatetime, _constraints),
    do: struct_match(value, NaiveDateTime)

  defp narrow_non_nil(value, Ash.Type.Duration, _constraints), do: struct_match(value, Duration)

  defp narrow_non_nil(value, Ash.Type.Struct, constraints) do
    case constraints[:instance_of] do
      module when is_atom(module) -> struct_match(value, module)
      _ -> value
    end
  end

  defp narrow_non_nil(value, type, _constraints) do
    if Ash.Type.embedded_type?(type) do
      struct_match(value, type)
    else
      value
    end
  end

  defp allow_nil(value, narrowed, true) do
    quote generated: true do
      case unquote(value) do
        nil -> nil
        _ -> unquote(narrowed)
      end
    end
  end

  defp allow_nil(_value, narrowed, false), do: narrowed

  defp guarded(value, guard) do
    narrowed = Macro.var(:narrowed, __MODULE__)

    quote generated: true do
      case unquote(value) do
        unquote(narrowed) when unquote({guard, [], [narrowed]}) -> unquote(narrowed)
      end
    end
  end

  defp struct_match(value, module) do
    narrowed = Macro.var(:narrowed, __MODULE__)

    quote generated: true do
      case unquote(value) do
        %unquote(module){} = unquote(narrowed) -> unquote(narrowed)
      end
    end
  end

  defp constrained_map(value, fields) do
    {:%{}, [],
     Enum.map(fields, fn {name, constraints} ->
       field_value =
         quote generated: true do
           unquote(value)[unquote(name)]
         end

       {name,
        narrow_type(
          field_value,
          constraints[:type] || Ash.Type.Term,
          constraints[:constraints] || [],
          Keyword.get(constraints, :allow_nil?, true)
        )}
     end)}
  end

  defp managed_relationship_inputs(resource, action) do
    action
    |> Map.get(:changes, [])
    |> Enum.reduce(%{}, fn
      %{change: {Ash.Resource.Change.ManageRelationship, change_opts}}, inputs ->
        argument = change_opts[:argument]
        relationship_name = change_opts[:relationship]
        relationship = Ash.Resource.Info.relationship(resource, relationship_name)
        fields = managed_relationship_fields(relationship, change_opts[:opts] || [])
        Map.update(inputs, argument, fields, &merge_fields(&1, fields))

      _, inputs ->
        inputs
    end)
  end

  defp managed_relationship_fields(nil, _opts), do: []

  defp managed_relationship_fields(relationship, opts) do
    resources =
      [relationship.destination]
      |> then(fn resources ->
        if Map.get(relationship, :type) == :many_to_many do
          [relationship.through | resources]
        else
          resources
        end
      end)

    if Enum.all?(resources, &Ash.Resource.Info.resource?/1) do
      do_managed_relationship_fields(relationship, opts)
    else
      []
    end
  end

  defp do_managed_relationship_fields(relationship, opts) do
    helpers = Ash.Changeset.ManagedRelationshipHelpers

    opts =
      case opts[:type] do
        nil -> opts
        type -> Keyword.merge(Ash.Changeset.manage_relationship_opts(type), opts)
      end

    opts = helpers.sanitize_opts(relationship, opts)

    action_refs =
      [
        helpers.on_no_match_destination_actions(opts, relationship),
        helpers.on_match_destination_actions(opts, relationship),
        helpers.on_missing_destination_actions(opts, relationship),
        helpers.on_lookup_update_action(opts, relationship),
        helpers.on_lookup_read_action(opts, relationship)
      ]
      |> List.flatten()
      |> Enum.reject(&is_nil/1)

    action_fields =
      Enum.flat_map(action_refs, fn
        {:destination, action_name} ->
          action_input_fields(relationship.destination, action_name)

        {:join, action_name, :*} ->
          action_input_fields(relationship.through, action_name)

        {:join, action_name, keys} ->
          relationship.through
          |> action_input_fields(action_name)
          |> Enum.filter(&(&1.name in keys))
      end)

    identity_fields =
      relationship
      |> Ash.Actions.ManagedRelationships.pkeys(opts)
      |> List.flatten()
      |> Enum.uniq()
      |> Enum.flat_map(fn name ->
        case Ash.Resource.Info.attribute(relationship.destination, name) do
          nil -> []
          attribute -> [attribute]
        end
      end)

    merge_fields(action_fields, identity_fields)
  end

  defp action_input_fields(resource, action_name) do
    action = Ash.Resource.Info.action(resource, action_name)

    if action do
      arguments = Map.new(action.arguments, &{&1.name, &1})
      attributes = Map.new(Ash.Resource.Info.attributes(resource), &{&1.name, &1})

      resource
      |> Ash.Resource.Info.action_inputs(action_name)
      |> Enum.flat_map(fn name ->
        case arguments[name] || attributes[name] do
          nil -> []
          field -> [field]
        end
      end)
    else
      []
    end
  end

  defp merge_fields(left, right) do
    (left ++ right)
    |> Enum.group_by(& &1.name)
    |> Enum.map(fn {_name, [field | rest]} ->
      if Enum.all?(rest, &same_field_type?(&1, field)) do
        field
      else
        %{field | type: Ash.Type.Term, constraints: [], allow_nil?: true}
      end
    end)
  end

  defp same_field_type?(left, right) do
    left.type == right.type && left.constraints == right.constraints
  end

  defp argument_names(body) do
    {_body, names} =
      Macro.prewalk(body, [], fn
        {:argument, _, [name | _]} = argument, names when is_atom(name) ->
          {argument, [name | names]}

        ast, names ->
          {ast, names}
      end)

    Enum.reverse(names)
  end

  defp first_argument({:when, _, [head | _]}), do: first_argument(head)
  defp first_argument({_name, _, [first | _]}), do: first
  defp first_argument(_), do: nil

  defp variable_used?(body, {name, _, context}) do
    {_body, used?} =
      Macro.prewalk(body, false, fn
        {^name, _, ^context} = variable, _used? -> {variable, true}
        ast, used? -> {ast, used?}
      end)

    used?
  end

  defp put_line(ast, nil), do: ast

  defp put_line(ast, line) do
    Macro.prewalk(ast, fn
      {form, meta, args} -> {form, Keyword.put_new(meta, :line, line), args}
      other -> other
    end)
  end

  defp mark_user_code(ast) do
    Macro.prewalk(ast, fn
      {form, meta, args} -> {form, Keyword.put(meta, :generated, false), args}
      other -> other
    end)
  end
end
