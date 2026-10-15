# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals do
  @moduledoc """
  Typed signals that resources emit, and other resources listen to with their own actions.

  A signal module declares signals, and is the contract between the resources that emit them and
  the resources that listen to them:

  ```elixir
  defmodule MyApp.Shop.Signals do
    use Ash.Signals

    signals do
      signal :order_placed do
        argument :order_id, :uuid, allow_nil?: false
      end
    end
  end
  ```

  See the [signals guide](/documentation/topics/resources/signals.md) for how to emit and listen
  to signals, and the [Signals DSL documentation](dsl-ash-signals.html) for every option.
  """

  use Spark.Dsl, default_extensions: [extensions: [Ash.Signals.Dsl]]

  @type t :: module

  @typedoc """
  The signals that led to a listener running, starting with the one it received, followed by the
  signals that led to it. Listeners have it in the `signal_path` key of their context.

  Each entry has the signal's `signal_module` and `signal` name, and the `resource` and `action`
  that emitted it, which are `nil` for signals delivered by `Ash.Signals.Test.react_to_signal/4`.
  It doesn't contain the signals themselves, because a listener can receive many signals at once.
  """
  @type signal_path :: [
          %{
            signal_module: t(),
            signal: atom(),
            resource: Ash.Resource.t() | nil,
            action: atom() | nil
          }
        ]

  @doc """
  Emits a signal, delivering it to its listeners.

  Takes the changeset of a create, update or destroy action, or the input of a generic action.
  Must be called during the phase declared by the signal, by an action that declares that it
  emits the signal dynamically:

  ```elixir
  create :place do
    emit MyApp.Shop.Signals, :order_placed, dynamic?: true
    change MyApp.Shop.Changes.EmitOrderPlaced
  end
  ```

  How each listener receives the signal depends on its `delivery`. By default, listeners run
  immediately. See `Ash.Signals.Delivery`.

  Every signal has a `signal_id`, a UUIDv7 generated when it is emitted. Include `signal_id` in
  the payload to set it yourself, for example to emit the same signal again.

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
  Emits a signal once for each payload, delivering them to its listeners together.

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

  Every payload is cast before the signals are delivered, so an invalid payload emits nothing.
  With the default delivery, each listener then handles every signal before the next listener
  runs.

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

    if !Enum.any?(
         (subject.action && Map.get(subject.action, :emits)) || [],
         &(&1.signal_module == signal_module and &1.signal == name)
       ) do
      raise ArgumentError, """
      Action `#{inspect(subject.action && subject.action.name)}` of #{inspect(subject.resource)} does not emit `#{inspect(name)}` from #{inspect(signal_module)}.

      Declare it on the action:

          emit #{inspect(signal_module)}, #{inspect(name)}, dynamic?: true
      """
    end

    if subject.phase != signal.phase do
      raise ArgumentError,
            "Signal `#{inspect(name)}` from #{inspect(signal_module)} can only be emitted during `#{inspect(signal.phase)}`, but the #{if struct == Ash.Changeset, do: "changeset", else: "action input"} is in `#{inspect(subject.phase)}`."
    end

    with {:ok, signal_structs} <- cast_payloads(signal, payloads) do
      context = context(subject, signal_module, signal, opts)

      Enum.each(signal_structs, &emit_telemetry(signal_module, name, &1, context))

      signal_module
      |> Ash.Signals.Registry.listeners(name)
      |> deliver(signal_structs, context)
    end
  end

  @doc false
  # Delivers signals to their listeners with no emitting action, for
  # `Ash.Signals.Test.react_to_signal/4`.
  def react(signal_module, name, payloads, opts) do
    signal = fetch_signal!(signal_module, name)
    opts = Ash.Actions.Helpers.apply_scope_to_opts(opts)

    listeners =
      signal_module
      |> Ash.Signals.Registry.listeners(name)
      |> only_listeners(opts[:only], signal_module, name)

    with {:ok, signal_structs} <- cast_payloads(signal, payloads) do
      context =
        struct!(
          Ash.Signals.Delivery.Context,
          opts
          |> Keyword.take([:actor, :tenant, :authorize?, :tracer])
          |> Keyword.merge(
            signal_module: signal_module,
            signal: signal,
            source_context: opts[:context] || %{},
            signal_path: signal_path([], signal_module, name, nil, nil)
          )
        )

      if opts[:inline?] do
        Ash.Signals.Delivery.Inline.deliver(signal_structs, listeners, [], context)
      else
        deliver(listeners, signal_structs, context)
      end
    end
  end

  defp only_listeners(listeners, nil, _signal_module, _name), do: listeners

  defp only_listeners(listeners, only, signal_module, name) do
    only = List.wrap(only)

    case Enum.filter(listeners, &(&1.resource in only or {&1.resource, &1.name} in only)) do
      [] ->
        raise ArgumentError,
              "No listener of `#{inspect(name)}` from #{inspect(signal_module)} matches `only: #{inspect(only)}`."

      listeners ->
        listeners
    end
  end

  # Each delivery is called once, with the listeners that use it with the same options.
  defp deliver(listeners, signal_structs, context) do
    listeners
    |> Enum.map(& &1.delivery)
    |> Enum.uniq()
    |> Enum.reduce_while(:ok, fn {delivery, opts} = key, :ok ->
      listeners = Enum.filter(listeners, &(&1.delivery == key))

      case delivery.deliver(signal_structs, listeners, opts, context) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  @doc """
  Runs a listener for the given signals, which is what the default delivery does for each
  listener.

  Deliveries that run listeners later, for example in a job, call this to run them the same way.
  Takes the options of the action the listener runs: `:actor`, `:tenant`, `:authorize?`,
  `:tracer` and `:context`, which should contain the `signal_path` of the delivery context.
  """
  @spec run_listener(Ash.Resource.SignalListener.t(), [struct()], Keyword.t()) ::
          :ok | {:error, term()}
  def run_listener(%{action_type: :action, batch?: true} = listener, signal_structs, opts) do
    run_generic(listener, signal_structs, opts)
  end

  def run_listener(%{action_type: :action} = listener, signal_structs, opts) do
    Enum.reduce_while(signal_structs, :ok, fn signal_struct, :ok ->
      case run_generic(listener, signal_struct, opts) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  def run_listener(%{action_type: :create} = listener, signal_structs, opts) do
    signal_structs
    |> Enum.map(&listener_inputs(listener, &1))
    |> Ash.bulk_create(listener.resource, listener.action, bulk_opts(opts))
    |> bulk_result()
  end

  def run_listener(%{action_type: type} = listener, signal_structs, opts)
      when type in [:update, :destroy] do
    listener
    |> listener_queries(signal_structs, opts)
    |> Enum.reduce_while(:ok, fn {query, inputs, _signal_structs}, :ok ->
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

  @doc """
  Returns the queries that select the records an update or destroy listener runs on.

  Signals are grouped by the arguments they give the listener's read action and the inputs they
  give its action. Returns a `{query, inputs, signals}` tuple for each group.
  """
  @spec listener_queries(Ash.Resource.SignalListener.t(), [struct()], Keyword.t()) ::
          [{Ash.Query.t(), map(), [struct()]}]
  def listener_queries(%{action_type: type} = listener, signal_structs, opts)
      when type in [:update, :destroy] do
    signal_structs
    |> Enum.group_by(&{map_fields(listener.args, &1), listener_inputs(listener, &1)})
    |> Enum.map(fn {{args, inputs}, signal_structs} ->
      query =
        listener.resource
        |> Ash.Query.for_read(listener.read_action, args, opts)
        |> filter_get_by(listener.get_by, signal_structs)

      {query, inputs, signal_structs}
    end)
  end

  @doc """
  Dumps signal structs to maps that can be stored, for example in a job's arguments.

  Load them again with `load/3`.
  """
  @spec dump(t(), atom(), [struct()]) :: {:ok, [map()]} | {:error, term()}
  def dump(signal_module, name, signal_structs) do
    type = {:array, fetch_signal!(signal_module, name).struct}

    with {:ok, constraints} <- Ash.Type.init(type, []) do
      Ash.Type.dump_to_embedded(type, signal_structs, constraints)
    end
  end

  @doc """
  Loads signal structs from maps produced by `dump/3`.

  Fields the signal no longer has are ignored, and a required field that is missing is an error,
  so maps dumped by an older version of the signal load if they are still valid.
  """
  @spec load(t(), atom(), [map()]) :: {:ok, [struct()]} | {:error, term()}
  def load(signal_module, name, dumped) do
    type = {:array, fetch_signal!(signal_module, name).struct}

    with {:ok, constraints} <- Ash.Type.init(type, []),
         {:ok, signal_structs} <- Ash.Type.cast_stored(type, dumped, constraints) do
      Ash.Type.apply_constraints(type, signal_structs, constraints)
    end
  end

  @doc """
  Dumps a `t:signal_path/0` to maps of strings that can be stored, for example in a job's
  arguments.

  Load it again with `load_signal_path/1`.
  """
  @spec dump_signal_path(signal_path()) :: [map()]
  def dump_signal_path(signal_path) do
    Enum.map(signal_path, fn entry ->
      %{
        "signal_module" => Atom.to_string(entry.signal_module),
        "signal" => Atom.to_string(entry.signal),
        "resource" => entry.resource && Atom.to_string(entry.resource),
        "action" => entry.action && Atom.to_string(entry.action)
      }
    end)
  end

  @doc """
  Loads a `t:signal_path/0` from maps produced by `dump_signal_path/1`.

  This creates the atoms the path refers to, so only load what `dump_signal_path/1` produced,
  from storage you trust.
  """
  @spec load_signal_path([map()]) :: signal_path()
  # The names were dumped from atoms by `dump_signal_path/1`, so they are bounded by the
  # application's own modules, signals and actions.
  # sobelow_skip ["DOS.StringToAtom"]
  def load_signal_path(dumped) do
    Enum.map(dumped, fn entry ->
      %{
        signal_module: String.to_atom(entry["signal_module"]),
        signal: String.to_atom(entry["signal"]),
        resource: entry["resource"] && String.to_atom(entry["resource"]),
        action: entry["action"] && String.to_atom(entry["action"])
      }
    end)
  end

  defp cast_payloads(signal, payloads) do
    payloads
    |> Enum.reduce_while({:ok, []}, fn payload, {:ok, signal_structs} ->
      case signal.struct.new(with_signal_id(Map.new(payload))) do
        {:ok, signal_struct} -> {:cont, {:ok, [signal_struct | signal_structs]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, signal_structs} -> {:ok, Enum.reverse(signal_structs)}
      {:error, error} -> {:error, error}
    end
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

  defp with_signal_id(payload) do
    if Map.has_key?(payload, :signal_id) or Map.has_key?(payload, "signal_id") do
      payload
    else
      Map.put(payload, :signal_id, Ash.UUIDv7.generate())
    end
  end

  # Without `inputs`, every signal field with the same name as an input of the action, except
  # `signal_id`, which is only set explicitly.
  defp listener_inputs(%{inputs: nil} = listener, signal_struct) do
    action_inputs = Ash.Resource.Info.action_inputs(listener.resource, listener.action)

    signal_struct
    |> Map.from_struct()
    |> Map.filter(fn {field, _} -> field != :signal_id and field in action_inputs end)
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

  defp emit_telemetry(signal_module, name, signal_struct, context) do
    :telemetry.execute(
      [:ash, :signal, :emitted],
      %{system_time: System.system_time()},
      %{
        signal_module: signal_module,
        name: name,
        signal: signal_struct,
        resource: context.resource,
        action: context.action,
        actor: context.actor,
        tenant: context.tenant
      }
    )
  end

  defp context(subject, signal_module, signal, opts) do
    private = subject.context[:private] || %{}
    opts = Ash.Actions.Helpers.apply_scope_to_opts(opts)

    shared_context = if shared = subject.context[:shared], do: %{shared: shared}, else: %{}

    defaults = [
      actor: private[:actor],
      tenant: subject.tenant,
      authorize?: private[:authorize?],
      tracer: private[:tracer]
    ]

    action = subject.action && subject.action.name

    struct!(
      Ash.Signals.Delivery.Context,
      defaults
      |> Keyword.merge(Keyword.take(opts, [:actor, :tenant, :authorize?, :tracer]))
      |> Keyword.merge(
        signal_module: signal_module,
        signal: signal,
        resource: subject.resource,
        action: action,
        source_context: Ash.Helpers.deep_merge_maps(shared_context, opts[:context] || %{}),
        signal_path:
          signal_path(
            subject.context[:signal_path] || [],
            signal_module,
            signal.name,
            subject.resource,
            action
          )
      )
    )
  end

  # When the emitting action is itself a listener, its path leads to the new signal.
  defp signal_path(path, signal_module, name, resource, action) do
    [%{signal_module: signal_module, signal: name, resource: resource, action: action} | path]
  end

  defp fetch_signal!(signal_module, name) do
    Ash.Signals.Info.signal(signal_module, name) ||
      raise ArgumentError, "#{inspect(signal_module)} has no signal named `#{inspect(name)}`."
  end
end
