# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals do
  @moduledoc """
  Typed signals that resources emit, and other resources listen to.

  A signal module is a contract between resources, often across domains. Emitters and
  listeners both depend on it, and not on each other.

  ```elixir
  defmodule MyApp.Blog.Signals do
    use Ash.Signals

    signals do
      signal :post_created do
        phase :after_action
        argument :post_id, :uuid, allow_nil?: false
      end
    end
  end
  ```

  Each signal defines a struct for its arguments, `MyApp.Blog.Signals.PostCreated`.

  Resources emit signals with the `emit_signal` change, or the `emit_signal` preparation for
  generic actions. These emit the signal during its phase, from the record's fields (or the
  action's arguments) with the same names, and are checked at compile time:

  ```elixir
  actions do
    create :create do
      change emit_signal(MyApp.Blog.Signals, :post_created, :after_action, fields: [post_id: :id])
    end
  end
  ```

  See `Ash.Resource.Change.Builtins.emit_signal/3` and
  `Ash.Resource.Preparation.Builtins.emit_signal/3`. To emit from your own code, use `emit/5`
  or `emit_many/5`, after listing the signal module in the resource's `signals_out`:

  ```elixir
  use Ash.Resource, signals_out: [MyApp.Blog.Signals]

  # in a change
  Ash.Changeset.after_action(changeset, fn changeset, post ->
    with :ok <-
           Ash.Signals.emit(changeset, MyApp.Blog.Signals, :post_created, %{post_id: post.id}) do
      {:ok, post}
    end
  end)
  ```

  Other resources listen with their own actions:

  ```elixir
  signals_in do
    # a generic action, which receives the signal struct as its `signal` argument
    on MyApp.Blog.Signals, :post_created, :notify_followers

    # a create action, with inputs from the signal fields of the same name
    on MyApp.Blog.Signals, :post_created, :create_feed_item

    # an update or destroy action, on the records a read action selects for each signal
    on MyApp.Blog.Signals, :post_archived, :archive, read_action: :for_post, args: [:post_id]
  end
  ```

  When signals are emitted together with `emit_many/5`, create, update and destroy listeners
  handle them as bulk actions, and a generic listener with `batch?: true` receives all of them as
  its `signals` argument. See the `on` entity of `signals_in` for every option.

  Listeners run immediately, as nested action calls, in the emitter's transaction. If a listener
  returns an error, `emit/5` returns it.

  Signals are fire and forget: the emitter does not get anything back from its listeners, so it
  does not depend on what they do, or on whether anything listens at all. If a resource needs a
  value from another resource to finish its action, it should call that resource directly.

  The `:ash_signals` compiler consolidates listeners, like protocol consolidation, so emitting a
  signal does not search for them:

      compilers: [:ash_signals] ++ Mix.compilers()

  If you use the Phoenix code reloader, add `:ash_signals` to your endpoint's
  `reloadable_compilers`. See `Mix.Tasks.Compile.AshSignals`. Without the compiler, listeners
  are searched for each time a signal is emitted.

  Each emitted signal fires a `[:ash, :signal, :emitted]` telemetry event, before its listeners
  run. Its metadata has the `:signal_module`, the signal's `:name`, the `:signal` struct, the
  emitting `:resource` and `:action` name, and the `:actor` and `:tenant` the listeners run with.

  To assert on emitted signals in tests, see `Ash.Signals.Test`.

  Listeners are found without either side depending on the other. See `Ash.Signals.Registry`.

  [Signals DSL documentation](dsl-ash-signals.html)
  """

  use Spark.Dsl, default_extensions: [extensions: [Ash.Signals.Dsl]]

  @type t :: module

  @doc """
  Emits a signal, running its listeners immediately.

  Takes the changeset of a create, update or destroy action, or the input of a generic action.
  Must be called during the phase declared by the signal, by a resource that lists the signal
  module in `signals_out`.

  ## Options

  * `:actor` - The actor to run the listeners as. Defaults to the emitting action's actor.
  * `:tenant` - Defaults to the emitting action's tenant.
  * `:authorize?` - Defaults to the emitting action's `authorize?`.
  * `:tracer` - Defaults to the emitting action's tracer.
  * `:context` - Context to pass to the listeners, merged into the emitting action's shared
    context.
  * `:scope` - A value implementing `Ash.Scope.ToOpts`. Any of the above that are also given
    explicitly take precedence over the scope.
  """
  @spec emit(
          Ash.Changeset.t() | Ash.ActionInput.t(),
          t(),
          atom(),
          map() | Keyword.t(),
          Keyword.t()
        ) ::
          :ok | {:error, term()}
  def emit(subject, signal_module, name, payload, opts \\ []) do
    emit_many(subject, signal_module, name, [payload], opts)
  end

  @doc """
  Emits a signal once for each payload, running its listeners immediately.

  Use this from the batch callbacks of a change, so that a bulk action emits its signals
  together. Pass any one of the batch's changesets as the subject, for example in
  `c:Ash.Resource.Change.after_batch/3`:

  ```elixir
  def after_batch([{changeset, _} | _] = changesets_and_posts, _opts, _context) do
    payloads = Enum.map(changesets_and_posts, fn {_, post} -> %{post_id: post.id} end)

    case Ash.Signals.emit_many(changeset, MyApp.Blog.Signals, :post_created, payloads) do
      :ok -> Enum.map(changesets_and_posts, fn {_, post} -> {:ok, post} end)
      {:error, error} -> [{:error, error}]
    end
  end
  ```

  Every payload is cast before any listener runs, so an invalid payload emits nothing. Each
  listener then handles every signal before the next listener runs.

  Takes the same options as `emit/5`.
  """
  @spec emit_many(
          Ash.Changeset.t() | Ash.ActionInput.t(),
          t(),
          atom(),
          [map() | Keyword.t()],
          Keyword.t()
        ) ::
          :ok | {:error, term()}
  def emit_many(subject, signal_module, name, payloads, opts \\ [])

  def emit_many(%struct{} = subject, signal_module, name, payloads, opts)
      when struct in [Ash.Changeset, Ash.ActionInput] do
    signal = fetch_signal!(signal_module, name)

    if signal_module not in Ash.Resource.Info.signals_out(subject.resource) do
      raise ArgumentError, """
      #{inspect(subject.resource)} cannot emit signals from #{inspect(signal_module)}.

      Add it to the resource:

          use Ash.Resource, signals_out: [#{inspect(signal_module)}]
      """
    end

    if subject.phase != signal.phase do
      raise ArgumentError,
            "Signal `#{inspect(name)}` from #{inspect(signal_module)} can only be emitted during `#{inspect(signal.phase)}`, but the #{if struct == Ash.Changeset, do: "changeset", else: "action input"} is in `#{inspect(subject.phase)}`."
    end

    with {:ok, signal_structs} <- cast_payloads(signal, payloads) do
      opts = opts(subject, opts)

      Enum.each(signal_structs, &emit_telemetry(subject, signal_module, name, &1, opts))
      run_listeners(signal_module, signal, signal_structs, opts)
    end
  end

  defp cast_payloads(signal, payloads) do
    payloads
    |> Enum.reduce_while({:ok, []}, fn payload, {:ok, signal_structs} ->
      case signal.struct.new(Map.new(payload)) do
        {:ok, signal_struct} -> {:cont, {:ok, [signal_struct | signal_structs]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, signal_structs} -> {:ok, Enum.reverse(signal_structs)}
      {:error, error} -> {:error, error}
    end
  end

  defp run_listeners(signal_module, signal, signal_structs, opts) do
    signal_module
    |> Ash.Signals.Registry.listeners(signal.name)
    |> Enum.reduce_while(:ok, fn listener, :ok ->
      case run_listener(listener, signal_structs, opts) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp run_listener(%{action_type: :action, batch?: true} = listener, signal_structs, opts) do
    run_generic(listener, signal_structs, opts)
  end

  defp run_listener(%{action_type: :action} = listener, signal_structs, opts) do
    Enum.reduce_while(signal_structs, :ok, fn signal_struct, :ok ->
      case run_generic(listener, signal_struct, opts) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp run_listener(%{action_type: :create} = listener, signal_structs, opts) do
    signal_structs
    |> Enum.map(&listener_inputs(listener, &1))
    |> Ash.bulk_create(listener.resource, listener.action, bulk_opts(opts))
    |> bulk_result()
  end

  defp run_listener(%{action_type: type} = listener, signal_structs, opts)
       when type in [:update, :destroy] do
    # one bulk action per distinct set of read action arguments and inputs
    signal_structs
    |> Enum.group_by(&{map_fields(listener.args, &1), listener_inputs(listener, &1)})
    |> Enum.reduce_while(:ok, fn {{args, inputs}, signal_structs}, :ok ->
      query =
        listener.resource
        |> Ash.Query.for_read(listener.read_action, args, opts)
        |> filter_get_by(listener.get_by, signal_structs)

      opts = Keyword.put(bulk_opts(opts), :strategy, [:atomic, :atomic_batches, :stream])

      result =
        case type do
          :update -> Ash.bulk_update(query, listener.action, inputs, opts)
          :destroy -> Ash.bulk_destroy(query, listener.action, inputs, opts)
        end

      case bulk_result(result) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp run_generic(listener, signal_or_signals, opts) do
    listener.resource
    |> Ash.ActionInput.for_action(
      listener.action,
      %{listener.argument => signal_or_signals},
      opts
    )
    |> Ash.run_action()
    |> case do
      {:error, error} -> {:error, error}
      _ -> :ok
    end
  end

  # Without `inputs`, every signal field with the same name as an input of the action.
  defp listener_inputs(%{inputs: nil} = listener, signal_struct) do
    action_inputs = Ash.Resource.Info.action_inputs(listener.resource, listener.action)

    signal_struct
    |> Map.from_struct()
    |> Map.filter(fn {field, _} -> field in action_inputs end)
  end

  defp listener_inputs(listener, signal_struct), do: map_fields(listener.inputs, signal_struct)

  defp map_fields(mapping, signal_struct) do
    Map.new(mapping, fn {name, field} -> {name, Map.fetch!(signal_struct, field)} end)
  end

  defp filter_get_by(query, [], _signal_structs), do: query

  defp filter_get_by(query, get_by, signal_structs) do
    Ash.Query.do_filter(query,
      or:
        signal_structs
        |> Enum.map(fn signal_struct ->
          Enum.map(get_by, fn {field, signal_field} ->
            {field, Map.fetch!(signal_struct, signal_field)}
          end)
        end)
        |> Enum.uniq()
    )
  end

  defp bulk_opts(opts) do
    Keyword.merge(opts,
      return_errors?: true,
      stop_on_error?: true,
      notify?: true,
      return_records?: false
    )
  end

  defp bulk_result(%Ash.BulkResult{status: :success}), do: :ok

  defp bulk_result(%Ash.BulkResult{errors: errors}),
    do: {:error, Ash.Error.to_error_class(errors || [])}

  defp emit_telemetry(subject, signal_module, name, signal_struct, opts) do
    :telemetry.execute(
      [:ash, :signal, :emitted],
      %{system_time: System.system_time()},
      %{
        signal_module: signal_module,
        name: name,
        signal: signal_struct,
        resource: subject.resource,
        action: subject.action && subject.action.name,
        actor: opts[:actor],
        tenant: opts[:tenant]
      }
    )
  end

  defp opts(subject, opts) do
    private = subject.context[:private] || %{}
    opts = Ash.Actions.Helpers.apply_scope_to_opts(opts)

    shared_context = if shared = subject.context[:shared], do: %{shared: shared}, else: %{}

    [
      actor: private[:actor],
      tenant: subject.tenant,
      authorize?: private[:authorize?],
      tracer: private[:tracer]
    ]
    |> Keyword.merge(Keyword.take(opts, [:actor, :tenant, :authorize?, :tracer]))
    |> Keyword.put(:context, Ash.Helpers.deep_merge_maps(shared_context, opts[:context] || %{}))
    |> Enum.reject(fn {key, value} -> is_nil(value) and key != :actor end)
  end

  defp fetch_signal!(signal_module, name) do
    Ash.Signals.Info.signal(signal_module, name) ||
      raise ArgumentError, "#{inspect(signal_module)} has no signal named `#{inspect(name)}`."
  end
end
