defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Create do
  @moduledoc false

  alias Ash.Reactor.CreateStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils
  import Reactor.Template, only: :macros

  @doc false
  @impl true
  def build(create, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, create) do
      initial =
        case create.initial do
          nil ->
            Argument.from_value(:initial, create.resource)

          module when is_atom(module) ->
            Argument.from_value(:initial, module)

          template when is_template(template) ->
            %Argument{name: :initial, source: template}
        end

      arguments =
        arguments
        |> maybe_append(create.actor)
        |> maybe_append(create.tenant)
        |> Enum.concat(create.wait_for)
        |> Enum.concat([initial])

      action_options =
        create
        |> Map.take([
          :action,
          :authorize?,
          :domain,
          :resource,
          :undo_action,
          :undo,
          :upsert_identity,
          :upsert?
        ])
        |> Enum.to_list()

      step_options =
        create
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        create.name,
        {CreateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_create, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(create, _dsl_state) when create.undo == :never, do: :ok

  def verify(create, dsl_state) do
    with {:ok, action} <- get_action(dsl_state, create.resource, create.undo_action),
         :ok <- verify_is_destroy_action(dsl_state, create.resource, action) do
      verify_action_takes_changeset(dsl_state, create.resource, action)
    end
  end

  defp get_action(dsl_state, resource, action_name) do
    case Info.action(resource, action_name) do
      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:create, :undo_action],
           message:
             "No action found matching the name `#{action_name}` on resource `#{inspect(resource)}`."
         )}

      action when is_struct(action) ->
        {:ok, action}
    end
  end

  defp verify_is_destroy_action(_dsl_state, _resource, action)
       when is_struct(action, Ash.Resource.Actions.Destroy),
       do: :ok

  defp verify_is_destroy_action(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:create, :undo_action],
       message: "The undo action for a create step should also be a destroy."
     )}
  end

  defp verify_action_takes_changeset(_dsl_state, _resource, %{arguments: [%{name: :changeset}]}),
    do: :ok

  defp verify_action_takes_changeset(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:create, :undo_action],
       message: "The undo action for an create step should take a single `changeset` argument."
     )}
  end
end
