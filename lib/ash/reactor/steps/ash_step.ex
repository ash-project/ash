defmodule Ash.Reactor.AshStep do
  @moduledoc """
  A reactor step which runs a step-module or an anonymous function, and enqueues any returned
  notifications before returning.

  The following return values are supported: `{:ok, result}`, `{:ok, result, notifications}`,
  `{:ok, result, notifications, new_steps}`

  Example:

  ```elixir
  ash_step :maybe_update_post do
    run fn %{post: post, new_amount_of_likes: new_amount_of_likes}, ctx ->
      opts = Ash.Context.to_opts(ctx, return_notifications?: true)
      if post.amount_of_likes != new_amount_of_likes do
        Post.update(post, %{amount_of_likes: new_amount_of_likes}, opts)
      else
        {:ok, post}
      end
  end
  ```
  ## Options

  * `run` - a one or two arity function or MFA which will be called as the run
    function of the step.
  * `compensate` - a one to three arity function or MFA which will be called as
    the compensate function of the step.  Optional.
  * `undo` - a one to three arity function or MFA which will be called as the
    undo function of this step.  Optional.
  """

  use Reactor.Step
  import Ash.Reactor.StepUtils

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), keyword) :: {:ok | :error, any}
  def run(arguments, context, options) do
    {module, step_opts} = Keyword.get(options, :impl) || {nil, []}

    if module do
      options =
        Enum.reduce(step_opts, options, fn {k, v}, acc ->
          maybe_set_kw(acc, k, v)
        end)

      module.run(arguments, context, options)
    else
      case Keyword.fetch!(options, :run) do
        fun when is_function(fun, 1) ->
          fun.(arguments)

        fun when is_function(fun, 2) ->
          fun.(arguments, context)

        {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [arguments, context] ++ a)
      end
    end
    |> case do
      {:ok, result, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result}

      {:ok, result, notifications, new_steps} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result, new_steps}

      result ->
        result
    end
  rescue
    error -> {:error, error}
  end

  @doc false
  @impl true
  @spec compensate(any, Reactor.inputs(), Reactor.context(), keyword) ::
          {:continue, any} | :ok | :retry
  def compensate(reason, arguments, context, options) do
    {module, step_opts} = Keyword.get(options, :impl) || {nil, []}

    if module do
      options =
        Enum.reduce(step_opts, options, fn {k, v}, acc ->
          maybe_set_kw(acc, k, v)
        end)

      module.compensate(reason, arguments, context, options)
    else
      case Keyword.fetch(options, :compensate) do
        {:ok, fun} when is_function(fun, 1) ->
          fun.(reason)

        {:ok, fun} when is_function(fun, 2) ->
          fun.(reason, arguments)

        {:ok, fun} when is_function(fun, 3) ->
          fun.(reason, arguments, context)

        {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [reason, arguments, context] ++ a)

        _ ->
          :ok
      end
    end
    |> case do
      {:ok, result, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result}

      {:ok, result, notifications, new_steps} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result, new_steps}

      result ->
        result
    end
  end

  @doc false
  @impl true
  @spec undo(any, Reactor.inputs(), Reactor.context(), keyword) :: :ok | :retry | {:error, any}
  def undo(value, arguments, context, options) do
    {module, step_opts} = Keyword.get(options, :impl) || {nil, []}

    if module do
      options =
        Enum.reduce(step_opts, options, fn {k, v}, acc ->
          maybe_set_kw(acc, k, v)
        end)

      module.undo(value, arguments, context, options)
    else
      case Keyword.fetch(options, :undo) do
        {:ok, fun} when is_function(fun, 1) ->
          fun.(value)

        {:ok, fun} when is_function(fun, 2) ->
          fun.(value, arguments)

        {:ok, fun} when is_function(fun, 3) ->
          fun.(value, arguments, context)

        {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
          apply(m, f, [value, arguments, context] ++ a)

        _ ->
          :ok
      end
    end
    |> case do
      {:ok, result, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result}

      {:ok, result, notifications, new_steps} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, result, new_steps}

      result ->
        result
    end
  end

  @doc false
  @impl true
  def can?(%{impl: {_, opts}}, :undo), do: is_function(Keyword.get(opts, :undo))
  def can?(%{impl: {_, opts}}, :compensate), do: is_function(Keyword.get(opts, :compensate))
  def can?(_, _), do: false
end
