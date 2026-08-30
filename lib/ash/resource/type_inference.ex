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
    for resource <- resources,
        change_module <- dispatched_change_modules(resource),
        definition = specialized_change_witness_definition(resource, change_module),
        definition != nil do
      %{
        key: {resource, change_module},
        fingerprint: :crypto.hash(:sha256, :erlang.term_to_binary(definition)),
        definition: definition
      }
    end
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

    clauses =
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.flat_map(fn action ->
        managed_inputs = managed_relationship_inputs(resource, action)

        action
        |> Map.get(:changes, [])
        |> Enum.flat_map(fn
          %{change: {^change_module, _opts}} ->
            arguments =
              typed_map(quote(do: raw_changeset), :arguments, action.arguments, managed_inputs)

            attributes = typed_map(quote(do: raw_changeset), :attributes, attributes)
            [local_witness_clause(action.name, arguments, attributes)]

          _ ->
            []
        end)
      end)
      |> Enum.uniq()

    with [_ | _] <- clauses,
         {:ok, definitions} <- change_definitions(change_module) do
      witness_module = Module.concat([Ash.TypeWitness, change_module, resource])

      definitions =
        definitions
        |> Enum.flat_map(&definition_ast/1)
        |> Enum.map(&rewrite_self_reference(&1, change_module, witness_module))
        |> Enum.map(&normalize_compiler_metadata/1)

      quote generated: false do
        defmodule unquote(witness_module) do
          unquote_splicing(definitions)
          unquote_splicing(clauses)
        end
      end
    else
      _ -> nil
    end
  end

  @doc false
  def dispatched_change_modules(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.flat_map(&Map.get(&1, :changes, []))
    |> Enum.flat_map(fn
      %{change: {module, _opts}} when is_atom(module) -> [module]
      _ -> []
    end)
    |> Enum.reject(&(&1 == Ash.Resource.Change.ManageRelationship))
    |> Enum.filter(fn module ->
      function_exported?(module, :change, 3) && match?({:ok, _}, change_definitions(module))
    end)
    |> Enum.uniq()
  end

  defp local_witness_clause(action_name, arguments, attributes) do
    quote generated: false do
      def __ash_type_witness__(unquote(action_name), raw_changeset, opts, context) do
        changeset = %{
          raw_changeset
          | arguments: unquote(arguments),
            attributes: unquote(attributes)
        }

        change(changeset, opts, context)
      end
    end
  end

  defp change_definitions(module) do
    if function_exported?(module, :__ash_type_inference_definitions__, 0) do
      {:ok, module.__ash_type_inference_definitions__()}
    else
      beam_definitions(module)
    end
  end

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

  defp narrow_non_nil(value, Ash.Type.Map, _constraints), do: guarded(value, :is_map)
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
