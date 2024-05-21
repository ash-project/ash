defmodule Ash.Reactor.Dsl.ActionTransformer do
  @moduledoc """
  Responsible for transforming actions.
  """

  alias Ash.{Domain, Resource}
  alias Reactor.Utils
  alias Spark.{Dsl, Dsl.Transformer, Error.DslError}

  use Transformer

  @doc false
  @impl true
  @spec before?(module) :: boolean
  def before?(Reactor.Dsl.Transformer), do: true
  def before?(_), do: false

  @doc false
  @impl true
  @spec transform(Dsl.t()) :: {:ok, Dsl.t()} | {:error, DslError.t()}
  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:reactor])
    |> Utils.reduce_while_ok(dsl_state, fn entity, dsl_state ->
      case transform_step(entity, dsl_state) do
        {:ok, entity, dsl_state} ->
          {:ok, Transformer.replace_entity(dsl_state, [:reactor], entity)}

        {:error, reason} ->
          {:error, reason}

        :ignore ->
          {:ok, dsl_state}
      end
    end)
  end

  defp transform_step(entity, dsl_state) when entity.action_step? do
    with {:ok, entity} <- transform_entity_domain(entity, dsl_state),
         :ok <- validate_entity_domain(entity, dsl_state),
         :ok <- validate_entity_resource(entity, dsl_state),
         {:ok, action} <- get_entity_resource_action(entity, dsl_state),
         entity <- %{entity | action: action.name},
         :ok <- validate_entity_input_names(entity, action, dsl_state),
         :ok <- validate_entity_input_dupes(entity, dsl_state),
         :ok <- validate_entity_input_empty(entity, dsl_state),
         :ok <- maybe_validate_upsert_identity(entity, dsl_state) do
      transform_nested_steps(entity, dsl_state)
    end
  end

  defp transform_step(entity, dsl_state) when entity.type == :transaction do
    with :ok <- validate_entity_resources(entity, dsl_state) do
      transform_nested_steps(entity, dsl_state)
    end
  end

  defp transform_step(_entity, _dsl_state), do: :ignore

  defp transform_nested_steps(entity, dsl_state) when is_list(entity.steps) do
    entity.steps
    |> Enum.reduce_while({:ok, %{entity | steps: []}, dsl_state}, fn
      step, {:ok, entity, dsl_state} ->
        case transform_step(step, dsl_state) do
          {:ok, step, dsl_state} ->
            {:cont, {:ok, %{entity | steps: [step | entity.steps]}, dsl_state}}

          :ignore ->
            {:cont, {:ok, %{entity | steps: [step | entity.steps]}, dsl_state}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
    end)
    |> case do
      {:ok, entity, dsl_state} -> {:ok, %{entity | steps: Enum.reverse(entity.steps)}, dsl_state}
      other -> other
    end
  end

  defp transform_nested_steps(entity, dsl_state),
    do: {:ok, entity, dsl_state}

  defp transform_entity_domain(entity, dsl_state) do
    resource_domain = Resource.Info.domain(entity.resource)
    default_domain = Transformer.get_option(dsl_state, [:ash], :default_domain)

    {:ok, %{entity | domain: entity.domain || resource_domain || default_domain}}
  end

  defp validate_entity_domain(entity, dsl_state) do
    if entity.domain.spark_is() == Domain do
      :ok
    else
      {:error,
       DslError.exception(
         module: Transformer.get_entities(dsl_state, :module),
         path: [:reactor, entity.type, entity.name],
         message:
           "The #{entity.type} step `#{inspect(entity.name)}` has its domain set to `#{inspect(entity.domain)}` but it is not a valid `Ash.Domain`."
       )}
    end
  end

  defp validate_entity_resource(entity, dsl_state) do
    if entity.resource.spark_is() == Resource do
      :ok
    else
      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         path: [:reactor, entity.type, entity.name],
         message:
           "The #{entity.type} step `#{inspect(entity.name)}` has its resource set to `#{inspect(entity.resource)}` but it is not a valid Ash resource."
       )}
    end
  end

  defp validate_entity_resources(entity, dsl_state) do
    entity.resources
    |> Enum.reject(&(&1.spark_is() == Resource))
    |> case do
      [] ->
        :ok

      [resource] ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message:
             "The #{entity.type} step `#{inspect(entity.name)}` has its resources set to `#{inspect(resource)}` but it is not a valid Ash resource."
         )}

      resources ->
        resources = Enum.map_join(resources, ", ", &"`#{inspect(&1)}`")

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message:
             "The #{entity.type} step `#{inspect(entity.name)}` has its resources set to #{resources} but they are not valid Ash resources."
         )}
    end
  end

  defp validate_entity_input_dupes(%{inputs: [_]} = _entity, _dsl_state), do: :ok

  defp validate_entity_input_dupes(%{inputs: [_ | _]} = entity, dsl_state) do
    entity.inputs
    |> Enum.map(&MapSet.new(Map.keys(&1.template)))
    |> Enum.reduce(&MapSet.intersection/2)
    |> Enum.to_list()
    |> case do
      [] ->
        :ok

      [key] ->
        message = """
        The #{entity.type} step `#{inspect(entity.name)}` defines multiple inputs for `#{key}`.
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}

      keys ->
        keys_sentence =
          keys
          |> Enum.map(&"`#{&1}`")
          |> to_sentence(final_sep: " and ")

        message = """
        The #{entity.type} step `#{inspect(entity.name)}` defines multiple inputs for the keys #{keys_sentence}.
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}
    end
  end

  defp validate_entity_input_dupes(_entity, _dsl_state), do: :ok

  defp validate_entity_input_empty(entity, dsl_state) when is_map_key(entity, :inputs) do
    entity.inputs
    |> Enum.filter(&Enum.empty?(&1.template))
    |> case do
      [] ->
        :ok

      [_] ->
        message = """
        The #{entity.type} step `#{inspect(entity.name)}` defines an empty input template.
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}

      _ ->
        message = """
        The #{entity.type} step `#{inspect(entity.name)}` defines empty input templates.
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}
    end
  end

  defp validate_entity_input_empty(_, _), do: :ok

  defp validate_entity_input_names(entity, action, dsl_state) when is_map_key(entity, :inputs) do
    argument_names = Enum.map(action.arguments, & &1.name)

    allowed_input_names =
      entity.resource
      |> Resource.Info.attributes()
      |> Enum.map(& &1.name)
      |> maybe_accept_inputs(action)
      |> maybe_reject_inputs(action)
      |> Enum.concat(argument_names)
      |> MapSet.new()

    provided_input_names =
      entity.inputs
      |> Enum.flat_map(&Map.keys(&1.template))
      |> MapSet.new()

    provided_input_names
    |> MapSet.difference(allowed_input_names)
    |> Enum.to_list()
    |> case do
      [] ->
        :ok

      [extra] ->
        suggestions =
          allowed_input_names
          |> Enum.map(&to_string/1)
          |> sorted_suggestions(extra)

        message = """
        The #{entity.type} step `#{inspect(entity.name)} refers to an input named `#{extra}` which doesn't exist.

        #{suggestions}
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}

      extras ->
        suggestions =
          allowed_input_names
          |> Enum.map(&to_string/1)
          |> sorted_suggestions(hd(extras))

        extras_sentence =
          extras
          |> Enum.map(&"`#{&1}`")
          |> to_sentence(final_sep: " and ")

        message = """
        The #{entity.type} step `#{inspect(entity.name)} refers to an inputs named #{extras_sentence} which don't exist.

        #{suggestions}
        """

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message: message
         )}
    end
  end

  defp validate_entity_input_names(_entity, _action, _dsl_state), do: :ok

  defp maybe_accept_inputs(input_names, action) when length(action.accepts) > 0,
    do: Enum.filter(input_names, &(&1 in action.accepts))

  defp maybe_accept_inputs(input_names, _), do: input_names

  defp maybe_reject_inputs(input_names, action) when length(action.rejects) > 0,
    do: Enum.filter(input_names, &(&1 in action.rejects))

  defp maybe_reject_inputs(input_names, _), do: input_names

  defp get_entity_resource_action(entity, dsl_state) when is_nil(entity.action) do
    entity.resource
    |> Resource.Info.actions()
    |> Enum.find(&(&1.type == entity.type && &1.primary?))
    |> case do
      nil ->
        suggestions =
          entity.resource
          |> Resource.Info.actions()
          |> Enum.filter(&(&1.type == entity.type))
          |> Enum.map(&to_string(&1.name))
          |> sorted_suggestions(entity.action,
            prefix: "Available #{entity.type} actions are ",
            suffix: ".",
            final_sep: " and "
          )

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message:
             "The step `#{inspect(entity.name)}` has no action name specified and no primary #{entity.type} action." <>
               suggestions
         )}

      action ->
        {:ok, action}
    end
  end

  defp get_entity_resource_action(entity, dsl_state) do
    case Resource.Info.action(entity.resource, entity.action, action_type(entity.type)) do
      nil ->
        suggestions =
          entity.resource
          |> Resource.Info.actions()
          |> Enum.filter(&(&1.type == entity.type))
          |> Enum.map(&to_string(&1.name))
          |> sorted_suggestions(entity.action)

        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:reactor, entity.type, entity.name],
           message:
             "The #{entity.type} step `#{inspect(entity.name)}` refers to an action named `#{entity.action}` which doesn't exist." <>
               suggestions
         )}

      action ->
        {:ok, action}
    end
  end

  defp action_type(:bulk_create), do: :create
  defp action_type(:bulk_update), do: :update
  defp action_type(:create), do: :create
  defp action_type(:read), do: :read
  defp action_type(:update), do: :update
  defp action_type(:destroy), do: :destroy
  defp action_type(:action), do: :action

  defp maybe_validate_upsert_identity(entity, dsl_state)
       when entity.upsert? and entity.upsert_identity do
    if Resource.Info.identity(entity.resource, entity.upsert_identity) do
      :ok
    else
      suggestions =
        entity.resource
        |> Resource.Info.identities()
        |> Enum.map(& &1.name)
        |> sorted_suggestions(entity.upsert_identity)

      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         path: [:reactor, entity.type, entity.name],
         message:
           "The #{entity.type} step `#{inspect(entity.name)}` refers to an identity named `#{entity.upsert_identity}` but it does not exist." <>
             suggestions
       )}
    end
  end

  defp maybe_validate_upsert_identity(_entity, _dsl_state), do: :ok

  defp sorted_suggestions(suggestions, tried, options \\ [])
  defp sorted_suggestions([], _, _), do: ""

  defp sorted_suggestions(suggestions, tried, options) do
    tried = to_string(tried)

    prefix = Keyword.get(options, :prefix, "Did you mean ")
    suffix = Keyword.get(options, :suffix, "?")

    suggestions
    |> Enum.map(&to_string/1)
    |> Enum.sort_by(&String.jaro_distance(&1, tried))
    |> Enum.map(&"`#{&1}`")
    |> to_sentence(options)
    |> then(&"\n\n#{prefix}#{&1}#{suffix}")
  end

  defp to_sentence([value], _opts), do: to_string(value)

  defp to_sentence([_ | _] = values, opts) do
    [last | rest] = Enum.reverse(values)

    sep = Keyword.get(opts, :sep, ", ")
    final_sep = Keyword.get(opts, :final_sep, " or ")

    rest =
      rest
      |> Enum.reverse()
      |> Enum.join(sep)

    "#{rest}#{final_sep}#{last}"
  end
end
