defmodule Ash.Resource.Transformers.ExtendActions do
  @moduledoc "Resolves `extends` option on actions by merging configuration from the base action."
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def before?(Ash.Resource.Transformers.RequireUniqueActionNames), do: true
  def before?(Ash.Resource.Transformers.SetPrimaryActions), do: true
  def before?(Ash.Resource.Transformers.DefaultAccept), do: true
  def before?(Ash.Resource.Transformers.GetByReadActions), do: true
  def before?(_), do: false

  @list_fields %{
    create:
      ~w(arguments changes metadata reject require_attributes allow_nil_input notifiers touches_resources skip_unknown_inputs)a,
    read: ~w(arguments preparations filters metadata touches_resources skip_unknown_inputs)a,
    update:
      ~w(arguments changes metadata reject require_attributes allow_nil_input notifiers touches_resources skip_unknown_inputs atomics)a,
    destroy:
      ~w(arguments changes metadata reject require_attributes allow_nil_input notifiers touches_resources skip_unknown_inputs)a,
    action: ~w(arguments preparations touches_resources skip_unknown_inputs)a
  }

  @excluded_fields [:name, :primary?, :type, :extends, :__spark_metadata__]

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.filter(& &1.extends)
    |> Enum.reduce_while({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      all_actions = Transformer.get_entities(dsl_state, [:actions])
      base = Enum.find(all_actions, &(&1.name == action.extends))

      cond do
        is_nil(base) ->
          {:halt,
           {:error,
            DslError.exception(
              module: Transformer.get_persisted(dsl_state, :module),
              path: [:actions, action.type],
              message:
                "Action `#{action.name}` extends `#{action.extends}`, but no action named `#{action.extends}` exists."
            )}}

        base.type != action.type ->
          {:halt,
           {:error,
            DslError.exception(
              module: Transformer.get_persisted(dsl_state, :module),
              path: [:actions, action.type],
              message:
                "Action `#{action.name}` (#{action.type}) extends `#{action.extends}` (#{base.type}), but they must be the same type."
            )}}

        true ->
          merged = merge_action(action, base)

          new_state =
            Transformer.replace_entity(
              dsl_state,
              [:actions],
              merged,
              &(&1.name == action.name && &1.type == action.type)
            )

          {:cont, {:ok, new_state}}
      end
    end)
  end

  defp merge_action(extending, base) do
    list_fields = Map.get(@list_fields, extending.type, [])
    explicitly_set = explicitly_set_fields(extending)

    extending
    |> Map.keys()
    |> Enum.reject(&(&1 in @excluded_fields))
    |> Enum.reduce(extending, fn field, acc ->
      if field in list_fields do
        Map.put(acc, field, Map.get(base, field, []) ++ Map.get(acc, field, []))
      else
        if field in explicitly_set do
          acc
        else
          Map.put(acc, field, Map.get(base, field))
        end
      end
    end)
  end

  defp explicitly_set_fields(entity) do
    case entity do
      %{__spark_metadata__: %{properties_anno: props}} when props != %{} ->
        props
        |> Map.keys()
        |> Enum.reject(&(&1 in @excluded_fields))

      _ ->
        defaults = effective_defaults(entity)

        entity
        |> Map.keys()
        |> Enum.reject(&(&1 in @excluded_fields))
        |> Enum.filter(fn field ->
          Map.get(entity, field) != Map.get(defaults, field)
        end)
    end
  end

  defp effective_defaults(entity) do
    module = entity.__struct__
    struct_defaults = module.__struct__()
    opt_schema = module.opt_schema()

    opt_schema_defaults =
      opt_schema
      |> Enum.filter(fn {_key, opts} -> Keyword.has_key?(opts, :default) end)
      |> Map.new(fn {key, opts} -> {key, opts[:default]} end)

    Map.merge(struct_defaults, opt_schema_defaults)
  end
end
