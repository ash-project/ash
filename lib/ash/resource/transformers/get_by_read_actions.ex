defmodule Ash.Resource.Transformers.GetByReadActions do
  @moduledoc """
  Transform any read actions which contain a `get_by` option.
  """

  use Spark.Dsl.Transformer

  alias Ash.{Resource, Type}
  alias Spark.{Dsl, Dsl.Transformer, Error.DslError}

  @doc false
  @spec before?(module) :: boolean
  def before?(_), do: false

  @doc false
  @spec after?(module) :: boolean
  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false

  @doc false
  @spec transform(Dsl.t()) :: {:ok, Dsl.t()} | {:error, DslError.t()}
  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Stream.filter(&(&1.type == :read))
    |> Stream.reject(&(is_nil(&1.get_by) || &1.get_by == []))
    |> Enum.reduce_while({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      action = %{action | get_by: List.wrap(action.get_by)}

      with :ok <- validate_get_by_value(dsl_state, action),
           :ok <- validate_existing_arguments(dsl_state, action),
           {:ok, action} <- transform_action(dsl_state, action) do
        {:cont,
         {:ok,
          Transformer.replace_entity(
            dsl_state,
            [:actions],
            action,
            &(&1.name == action.name)
          )}}
      else
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp transform_action(dsl_state, action) do
    import Ash.Expr

    action =
      action.get_by
      |> Enum.reduce(%{action | get?: true}, fn field, action ->
        type = type_for_entity(dsl_state, field)

        arguments =
          if Enum.any?(action.arguments, &(&1.name == field)) do
            action.arguments
          else
            [
              Transformer.build_entity!(Resource.Dsl, [:actions, :read], :argument,
                name: field,
                type: type
              )
              | action.arguments
            ]
          end

        filter =
          case action.filter do
            nil -> expr(^ref(field) == ^arg(field))
            filter -> where(^filter, ^ref(field) == ^arg(field))
          end

        %{action | arguments: arguments, filter: filter}
      end)

    {:ok, action}
  end

  defp type_for_entity(dsl_state, field) do
    []
    |> Stream.concat(Transformer.get_entities(dsl_state, [:attributes]))
    |> Stream.concat(Transformer.get_entities(dsl_state, [:calculations]))
    |> Stream.concat(Transformer.get_entities(dsl_state, [:aggregates]))
    |> Enum.find(&(&1.name == field))
    |> case do
      aggregate when is_struct(aggregate, Resource.Aggregate) ->
        {:ok, type} = Resource.Info.aggregate_type(dsl_state, aggregate)
        Type.get_type(type)

      other ->
        Type.get_type(other.type)
    end
  end

  defp validate_get_by_value(dsl_state, action) do
    attributes = map_entities(dsl_state, [:attributes], & &1.filterable?)
    calculations = map_entities(dsl_state, [:calculations], & &1.filterable?)
    aggregates = map_entities(dsl_state, [:aggregates], & &1.filterable?)

    action.get_by
    |> Enum.reduce_while(:ok, fn get_by, _ ->
      cond do
        Map.has_key?(attributes, get_by) ->
          if Map.get(attributes, get_by),
            do: {:cont, :ok},
            else: {:halt, is_not_filterable_error(dsl_state, action, :attribute, get_by)}

        Map.has_key?(calculations, get_by) ->
          if Map.get(calculations, get_by),
            do: {:cont, :ok},
            else: {:halt, is_not_filterable_error(dsl_state, action, :calculation, get_by)}

        Map.has_key?(aggregates, get_by) ->
          if Map.get(aggregates, get_by),
            do: {:cont, :ok},
            else: {:halt, is_not_filterable_error(dsl_state, action, :aggregate, get_by)}

        true ->
          {:halt,
           {:error,
            dsl_error(
              dsl_state,
              [:actions, :read, action.name, :get_by],
              "`#{inspect(get_by)}` is not a valid attribute, calculation or aggregate"
            )}}
      end
    end)
  end

  defp validate_existing_arguments(_dsl_state, action) when action.arguments == [], do: :ok

  defp validate_existing_arguments(dsl_state, action) do
    attributes = map_entities(dsl_state, [:attributes], &Type.get_type(&1.type))
    calculations = map_entities(dsl_state, [:calculations], &Type.get_type(&1.type))

    aggregates =
      map_entities(dsl_state, [:aggregates], fn aggregate ->
        case Resource.Info.aggregate_type(dsl_state, aggregate) do
          {:ok, type} -> Type.get_type(type)
          {:error, _reason} -> nil
        end
      end)

    action.arguments
    |> Stream.filter(&Enum.member?(action.get_by, &1))
    |> Enum.reduce_while(:ok, fn argument, _ ->
      argument_type = Type.get_type(argument.type)

      cond do
        Map.has_key?(attributes, argument.name) ->
          attribute_type = Map.get(attributes, argument.name)

          if argument_type == attribute_type,
            do: {:cont, :ok},
            else:
              {:halt,
               types_do_not_match_error(
                 dsl_state,
                 action.name,
                 argument.name,
                 argument_type,
                 attribute_type,
                 :attribute
               )}

        Map.has_key?(calculations, argument.name) ->
          calculation_type = Map.get(calculations, argument.name)

          if argument_type == calculation_type,
            do: {:cont, :ok},
            else:
              {:halt,
               types_do_not_match_error(
                 dsl_state,
                 action.name,
                 argument.name,
                 argument_type,
                 calculation_type,
                 :calculation
               )}

        Map.has_key?(aggregates, argument.name) ->
          aggregate_type = Map.get(aggregates, argument.name)

          if argument_type == aggregate_type,
            do: {:cont, :ok},
            else:
              {:halt,
               types_do_not_match_error(
                 dsl_state,
                 action.name,
                 argument.name,
                 argument_type,
                 aggregate_type,
                 :aggregate
               )}
      end
    end)
  end

  defp types_do_not_match_error(
         dsl_state,
         action_name,
         argument_name,
         argument_type,
         property_type,
         property_type_type
       ) do
    {:error,
     dsl_error(
       dsl_state,
       [:actions, :read, action_name, :arguments, argument_name],
       "Type `#{inspect(argument_type)}` does not match the corresponding #{property_type_type} type (`#{inspect(property_type)}`)"
     )}
  end

  defp map_entities(dsl_state, path, mapper) when is_function(mapper, 1) do
    dsl_state
    |> Transformer.get_entities(path)
    |> Stream.map(&{&1.name, mapper.(&1)})
    |> Map.new()
  end

  defp is_not_filterable_error(dsl_state, action, type, name) do
    {:error,
     dsl_error(
       dsl_state,
       [:actions, :read, action.name, :get_by],
       "The #{type} `#{inspect(name)}` is not filterable, so cannot be used in a `get_by` action"
     )}
  end

  defp dsl_error(dsl_state, path, message) do
    DslError.exception(
      module: Transformer.get_persisted(dsl_state, :module),
      path: path,
      message: message
    )
  end
end
