# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateAttributeArgumentValidations do
  @moduledoc """
  Ensures builtin validations like `attribute_equals` and `argument_equals`
  refer to an actual attribute or argument.

  When the name exists as the other kind of input, the error suggests the
  matching builtin (for example `argument_equals` instead of `attribute_equals`).
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.{Entity, Verifier}
  alias Spark.Error.DslError

  @attribute_validations %{
    Ash.Resource.Validation.AttributeEquals =>
      %{name: "attribute_equals", suggest: "argument_equals"},
    Ash.Resource.Validation.AttributeDoesNotEqual =>
      %{name: "attribute_does_not_equal", suggest: "argument_does_not_equal"},
    Ash.Resource.Validation.AttributeIn => %{name: "attribute_in", suggest: "argument_in"},
    Ash.Resource.Validation.DataOneOf => %{name: "data_one_of", suggest: nil}
  }

  @argument_validations %{
    Ash.Resource.Validation.ArgumentEquals =>
      %{name: "argument_equals", suggest: "attribute_equals"},
    Ash.Resource.Validation.ArgumentDoesNotEqual =>
      %{name: "argument_does_not_equal", suggest: "attribute_does_not_equal"},
    Ash.Resource.Validation.ArgumentIn => %{name: "argument_in", suggest: "one_of"}
  }

  @impl true
  def verify(dsl_state) do
    context = %{
      module: Verifier.get_persisted(dsl_state, :module),
      attributes: attribute_names(dsl_state),
      actions: Ash.Resource.Info.actions(dsl_state)
    }

    errors =
      []
      |> check_entities(
        Ash.Resource.Info.validations(dsl_state),
        fn validation -> resource_argument_names(context, validation.on) end,
        [:validations],
        context
      )
      |> check_entities(
        Ash.Resource.Info.changes(dsl_state),
        fn change -> resource_argument_names(context, change.on) end,
        [:changes],
        context
      )
      |> check_entities(
        Verifier.get_entities(dsl_state, [:preparations]),
        fn preparation -> resource_argument_names(context, preparation.on) end,
        [:preparations],
        context
      )

    errors =
      Enum.reduce(context.actions, errors, fn action, errors ->
        check_entities(
          errors,
          action_entities(action),
          fn _ -> action_argument_names(action) end,
          [:actions, action.name],
          context
        )
      end)

    argument_names = all_argument_names(context.actions)

    errors =
      Enum.reduce(Ash.Resource.Info.pipelines(dsl_state), errors, fn pipeline, errors ->
        path = [:pipelines, pipeline.name]

        errors
        |> check_entities(pipeline.validations, fn _ -> argument_names end, path, context)
        |> check_entities(pipeline.changes, fn _ -> argument_names end, path, context)
        |> check_entities(pipeline.preparations, fn _ -> argument_names end, path, context)
      end)

    case errors do
      [] -> :ok
      errors -> {:error, Enum.reverse(errors)}
    end
  end

  defp attribute_names(dsl_state) do
    Ash.Resource.Info.attribute_names(dsl_state) ||
      dsl_state
      |> Ash.Resource.Info.attributes()
      |> MapSet.new(& &1.name)
  end

  defp action_entities(action) do
    Map.get(action, :changes, []) ++
      Map.get(action, :preparations, []) ++
      Map.get(action, :emits, [])
  end

  defp action_argument_names(action) do
    action
    |> Map.get(:arguments, [])
    |> MapSet.new(& &1.name)
  end

  defp all_argument_names(actions) do
    MapSet.new(Enum.flat_map(actions, fn action -> Enum.map(action.arguments, & &1.name) end))
  end

  defp resource_argument_names(context, types) do
    context.actions
    |> Enum.filter(&(&1.type in List.wrap(types)))
    |> all_argument_names()
  end

  defp check_entities(errors, entities, argument_names_fun, path, context) do
    Enum.reduce(entities, errors, fn entity, errors ->
      argument_names = argument_names_fun.(entity)
      location = Entity.anno(entity)

      entity
      |> validation_refs()
      |> Enum.reduce(errors, fn ref, errors ->
        case check_ref(ref, argument_names, path, location, context) do
          nil -> errors
          error -> [error | errors]
        end
      end)
    end)
  end

  defp validation_refs(%Ash.Resource.Validation{module: module, opts: opts, where: where}) do
    nested_refs({module, opts}) ++ Enum.flat_map(where, &nested_refs/1)
  end

  defp validation_refs(%{where: where}) when is_list(where) do
    Enum.flat_map(where, &nested_refs/1)
  end

  defp validation_refs(_), do: []

  defp nested_refs({Ash.Resource.Validation.All, opts}) do
    Enum.flat_map(List.wrap(opts[:validations]), &nested_refs/1)
  end

  defp nested_refs({Ash.Resource.Validation.Any, opts}) do
    Enum.flat_map(List.wrap(opts[:validations]), &nested_refs/1)
  end

  defp nested_refs({Ash.Resource.Validation.Negate, opts}) do
    nested_refs(opts[:validation])
  end

  defp nested_refs({module, opts}) when is_atom(module) and is_list(opts) do
    [{module, opts}]
  end

  defp nested_refs(module) when is_atom(module) and not is_nil(module) do
    [{module, []}]
  end

  defp nested_refs(_), do: []

  defp check_ref({module, opts}, argument_names, path, location, context) do
    cond do
      Map.has_key?(@attribute_validations, module) ->
        check_attribute(module, opts[:attribute], argument_names, path, location, context)

      Map.has_key?(@argument_validations, module) ->
        check_argument(module, opts[:argument], argument_names, path, location, context)

      true ->
        nil
    end
  end

  defp check_attribute(_module, name, _argument_names, _path, _location, _context)
       when not is_atom(name) or is_nil(name),
       do: nil

  defp check_attribute(module, name, argument_names, path, location, context) do
    if MapSet.member?(context.attributes, name) do
      nil
    else
      info = @attribute_validations[module]

      DslError.exception(
        module: context.module,
        path: path,
        location: location,
        message: attribute_message(info, name, argument_names, context)
      )
    end
  end

  defp check_argument(_module, name, _argument_names, _path, _location, _context)
       when not is_atom(name) or is_nil(name),
       do: nil

  defp check_argument(module, name, argument_names, path, location, context) do
    if MapSet.member?(argument_names, name) do
      nil
    else
      info = @argument_validations[module]

      DslError.exception(
        module: context.module,
        path: path,
        location: location,
        message: argument_message(info, name, path, context)
      )
    end
  end

  defp attribute_message(info, name, argument_names, context) do
    base =
      "`#{info.name}` refers to `#{inspect(name)}`, but `#{inspect(name)}` is not an attribute on `#{inspect(context.module)}`."

    cond do
      MapSet.member?(argument_names, name) and info.suggest ->
        """
        #{base}

        `#{inspect(name)}` is an argument. Did you mean `#{info.suggest}`?
        """

      MapSet.member?(argument_names, name) ->
        """
        #{base}

        `#{inspect(name)}` is an argument, and `#{info.name}` only validates attributes.
        """

      true ->
        other_actions = actions_with_argument(context.actions, name)

        if other_actions == [] do
          base
        else
          """
          #{base}

          `#{inspect(name)}` is an argument on #{describe_actions(other_actions)}. Did you mean `#{info.suggest || "an argument validation"}`?
          """
        end
    end
    |> String.trim()
  end

  defp argument_message(info, name, path, context) do
    base =
      "`#{info.name}` refers to `#{inspect(name)}`, but `#{inspect(name)}` is not an argument#{argument_scope(path)}."

    cond do
      MapSet.member?(context.attributes, name) and info.suggest ->
        """
        #{base}

        `#{inspect(name)}` is an attribute. Did you mean `#{info.suggest}`?
        """

      MapSet.member?(context.attributes, name) ->
        """
        #{base}

        `#{inspect(name)}` is an attribute.
        """

      true ->
        other_actions = actions_with_argument(context.actions, name)

        if other_actions == [] do
          "#{base} It is not an attribute either."
        else
          """
          #{base}

          `#{inspect(name)}` is an argument on #{describe_actions(other_actions)}.
          """
        end
    end
    |> String.trim()
  end

  defp argument_scope([:actions, action_name | _]), do: " on the `#{inspect(action_name)}` action"
  defp argument_scope(_path), do: " on this resource"

  defp actions_with_argument(actions, name) do
    Enum.filter(actions, fn action ->
      Enum.any?(action.arguments, &(&1.name == name))
    end)
  end

  defp describe_actions([action]) do
    "the `#{inspect(action.name)}` action"
  end

  defp describe_actions(actions) do
    names =
      actions
      |> Enum.map(&"`#{inspect(&1.name)}`")
      |> Enum.join(", ")

    "the #{names} actions"
  end
end
