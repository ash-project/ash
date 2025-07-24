defmodule Ash.Subject do
  @moduledoc """
  Provides a consistent API for common operations across `Ash.Changeset`,
  `Ash.Query`, and `Ash.ActionInput`. It allows you to write generic code that works
  with any of these types without needing to pattern match or special-case your logic.
  """

  @type t :: Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t()

  @typedoc """
  Function type for before action hooks.

  Receives an action input and returns a modified action input, optionally with notifications.
  """
  @type before_action_fun :: (t() ->
                                t() | {t(), %{notifications: [Ash.Notifier.Notification.t()]}})

  @typedoc """
  Function type for after action hooks.

  Receives the action input and the result of the action, and can return
  the result optionally with notifications, or an error.
  """
  @type after_action_fun ::
          (t(), term() ->
             {:ok, term()}
             | {:ok, term(), [Ash.Notifier.Notification.t()]}
             | {:error, any()})

  @typedoc """
  Function type for before transaction hooks.

  Receives an action input and returns a modified action input or an error.
  """
  @type before_transaction_fun :: (t() -> t() | {:error, any()})

  @typedoc """
  Function type for after transaction hooks.

  Receives the action input and the result of the transaction, and returns
  the result (potentially modified) or an error.
  """
  @type after_transaction_fun ::
          (t(), {:ok, term()} | {:error, any()} ->
             {:ok, term()} | {:error, any()})

  @typedoc """
  Function type for around transaction hooks.

  Receives an action input and a callback function that executes the transaction,
  and returns the result of calling the callback or an error.
  """
  @type around_transaction_fun ::
          (t(), (t() -> {:ok, term()} | {:error, any()}) ->
             {:ok, term()} | {:error, any()})

  @doc """
  Adds an error or list of errors to the subject.

  Supports all subject types (Changeset, Query, ActionInput) and maintains
  type consistency.

  ## Parameters

    * `subject` - The subject to add errors to
    * `errors` - Error or list of errors to add
  """
  @spec add_error(
          subject :: t(),
          error_input :: Ash.Error.error_input() | list(Ash.Error.error_input())
        ) :: t()
  def add_error(subject, []), do: subject

  def add_error(%Ash.Changeset{} = subject, error) do
    Ash.Changeset.add_error(subject, error, [])
  end

  def add_error(%Ash.Query{} = subject, error) do
    Ash.Query.add_error(subject, [], error)
  end

  def add_error(%Ash.ActionInput{} = subject, error) do
    Ash.ActionInput.add_error(subject, error, [])
  end

  @doc """
  Puts a key-value pair into the subject's context.

  ## Parameters

    * `subject` - The subject to update context on
    * `key` - The context key
    * `value` - The value to store
  """
  @spec put_context(
          subject :: t(),
          key :: atom(),
          value :: term()
        ) :: t()
  def put_context(subject, key, value) do
    set_context(subject, %{key => value})
  end

  @doc """
  Sets the context for the subject.

  Merges the provided map into the subject's existing context.
  For Changeset and Query, delegates to their specific implementations.

  ## Parameters

    * `subject` - The subject to set context on
    * `context` - Map of context data to merge
  """
  @spec set_context(subject :: t(), context :: map()) :: t()
  def set_context(%Ash.Changeset{} = subject, map) do
    Ash.Changeset.set_context(subject, map)
  end

  def set_context(%Ash.Query{} = subject, map) do
    Ash.Query.set_context(subject, map)
  end

  def set_context(subject, nil), do: subject

  def set_context(subject, map) do
    %{
      subject
      | context:
          subject.context
          |> Ash.Helpers.deep_merge_maps(map)
          |> then(&Ash.Helpers.deep_merge_maps(&1, map[:shared] || %{}))
    }
  end

  @doc """
  Gets an argument or attribute value from a Changeset, or just an argument from other subjects.

  For Changesets, this can retrieve both arguments and attributes.
  For Query and ActionInput, this only retrieves arguments.

  ## Parameters

    * `subject` - The subject to get value from
    * `name` - The argument or attribute name (atom or string)
  """
  @spec get_argument_or_attribute(
          subject :: t(),
          argument_or_attribute :: atom() | binary()
        ) :: term()
  def get_argument_or_attribute(subject, argument_or_attribute, default \\ nil)

  def get_argument_or_attribute(%Ash.Changeset{} = subject, argument_or_attribute, default) do
    case Ash.Changeset.get_argument_or_attribute(subject, argument_or_attribute) do
      nil -> default
      value -> value
    end
  end

  def get_argument_or_attribute(subject, argument_or_attribute, default) do
    get_argument(subject, argument_or_attribute, default)
  end

  @doc """
  Gets an argument value from the subject.

  Supports both atom and string argument names.

  ## Parameters

    * `subject` - The subject to get argument from
    * `argument` - The argument name (atom or string)
  """
  @spec get_argument(subject :: t(), argument :: atom() | binary(), default :: term()) :: term()
  def get_argument(subject, argument, default \\ nil)

  def get_argument(subject, argument, default) when is_atom(argument) do
    value =
      if Map.has_key?(subject.arguments, argument) do
        Map.get(subject.arguments, argument)
      else
        Map.get(subject.arguments, to_string(argument))
      end

    if is_nil(value) do
      default
    else
      value
    end
  end

  def get_argument(subject, argument, default) when is_binary(argument) do
    subject.arguments
    |> Enum.find(fn {key, _} ->
      to_string(key) == argument
    end)
    |> case do
      {_key, value} ->
        value

      _ ->
        default
    end
  end

  @spec get_attribute(subject :: t(), attribute :: atom()) :: term()
  def get_attribute(%Ash.Changeset{} = subject, attribute) do
    Ash.Changeset.get_attribute(subject, attribute)
  end

  def get_attribute(subject, attribute) do
    get_argument(subject, attribute)
  end

  @doc """
  Fetches an argument value from the subject.

  Returns `{:ok, value}` if the argument exists, `:error` otherwise.
  Supports both atom and string argument names.

  ## Parameters

    * `subject` - The subject to fetch argument from
    * `argument` - The argument name (atom or string)
  """
  @spec fetch_argument(subject :: t(), argument :: atom() | binary()) :: {:ok, term()} | :error
  def fetch_argument(subject, argument) when is_atom(argument) do
    case Map.fetch(subject.arguments, argument) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case Map.fetch(subject.arguments, to_string(argument)) do
          {:ok, value} -> {:ok, value}
          :error -> :error
        end
    end
  end

  def fetch_argument(subject, argument) when is_binary(argument) do
    subject.arguments
    |> Enum.find(fn {key, _} ->
      to_string(key) == argument
    end)
    |> case do
      {_key, value} ->
        {:ok, value}

      _ ->
        :error
    end
  end

  @doc """
  Sets multiple arguments on the subject.

  Takes a map of argument names to values and sets them all.

  ## Parameters

    * `subject` - The subject to set arguments on
    * `arguments` - Map of argument names to values
  """
  @spec set_arguments(subject :: t(), arguments :: map()) :: t()
  def set_arguments(subject, map) do
    Enum.reduce(map, subject, fn {key, value}, subject ->
      set_argument(subject, key, value)
    end)
  end

  @doc """
  Sets a single argument on the subject.

  ## Parameters

    * `subject` - The subject to set argument on
    * `argument` - The argument name (atom or string)
    * `value` - The value to set
  """
  @spec set_argument(subject :: t(), argument :: atom() | binary(), value :: term()) :: t()
  def set_argument(%Ash.Changeset{} = subject, argument, value) do
    Ash.Changeset.set_argument(subject, argument, value)
  end

  def set_argument(%Ash.Query{} = subject, argument, value) do
    Ash.Query.set_argument(subject, argument, value)
  end

  def set_argument(%Ash.ActionInput{} = subject, argument, value) do
    Ash.ActionInput.set_argument(subject, argument, value)
  end

  @doc """
  Deletes one or more arguments from the subject.

  ## Parameters

    * `subject` - The subject to delete arguments from
    * `arguments` - Single argument name or list of argument names to delete
  """
  @spec delete_argument(
          subject :: t(),
          argument_or_arguments :: atom() | binary() | list(atom() | binary())
        ) :: t()
  def delete_argument(%Ash.Changeset{} = subject, argument_or_arguments) do
    Ash.Changeset.delete_argument(subject, argument_or_arguments)
  end

  def delete_argument(%Ash.Query{} = subject, argument_or_arguments) do
    Ash.Query.delete_argument(subject, argument_or_arguments)
  end

  def delete_argument(subject, argument_or_arguments) do
    argument_or_arguments
    |> List.wrap()
    |> Enum.reduce(subject, fn argument, subject ->
      %{subject | arguments: Map.delete(subject.arguments, argument)}
    end)
  end

  @doc """
  Sets a private argument on the subject.

  Private arguments are not exposed in the public API.
  Only supported by Changeset and ActionInput.

  ## Parameters

    * `subject` - The subject to set private argument on (Changeset or ActionInput)
    * `argument` - The argument name (atom or string)
    * `value` - The value to set
  """
  @spec set_private_argument(
          subject :: Ash.Changeset.t() | Ash.ActionInput.t(),
          argument :: atom() | binary(),
          value :: term()
        ) ::
          Ash.Changeset.t() | Ash.ActionInput.t()
  def set_private_argument(%Ash.Changeset{} = subject, argument, value) do
    Ash.Changeset.set_private_argument(subject, argument, value)
  end

  def set_private_argument(%Ash.ActionInput{} = subject, argument, value) do
    Ash.ActionInput.set_private_argument(subject, argument, value)
  end

  @doc """
  Sets multiple private arguments on the subject.

  Takes a map of argument names to values and sets them all as private arguments.
  Only supported by Changeset and ActionInput.

  ## Parameters

    * `subject` - The subject to set private arguments on (Changeset or ActionInput)
    * `arguments` - Map of argument names to values
  """
  @spec set_private_arguments(
          subject :: Ash.Changeset.t() | Ash.ActionInput.t(),
          arguments :: map()
        ) ::
          Ash.Changeset.t() | Ash.ActionInput.t()
  def set_private_arguments(subject, map) do
    Enum.reduce(map, subject, fn {key, value}, subject ->
      set_private_argument(subject, key, value)
    end)
  end

  @doc """
  Adds a callback to be executed before the action.

  ## Parameters

    * `subject` - The subject to add callback to
    * `callback` - Function that takes and returns the subject
    * `opts` - Options including `:prepend?` to add at beginning
  """
  @spec before_action(
          subject :: t(),
          callback :: before_action_fun(),
          opts :: Keyword.t()
        ) :: t()
  def before_action(subject, callback, opts \\ [])

  def before_action(%Ash.Changeset{} = subject, callback, opts) do
    Ash.Changeset.before_action(subject, callback, opts)
  end

  def before_action(%Ash.Query{} = subject, callback, opts) do
    Ash.Query.before_action(subject, callback, opts)
  end

  def before_action(subject, callback, opts) do
    if opts[:prepend?] do
      %{subject | before_action: [callback | subject.before_action]}
    else
      %{subject | before_action: subject.before_action ++ [callback]}
    end
  end

  @doc """
  Adds a callback to be executed after the action.

  Note: Query only supports 2-arity callbacks and ignores opts.

  ## Parameters

    * `subject` - The subject to add callback to
    * `callback` - Function that processes the result
    * `opts` - Options including `:prepend?` (ignored for Query)
  """
  @spec after_action(
          subject :: t(),
          callback :: after_action_fun(),
          opts :: Keyword.t()
        ) :: t()
  def after_action(subject, callback, opts \\ [])

  def after_action(%Ash.Changeset{} = subject, callback, opts) do
    Ash.Changeset.after_action(subject, callback, opts)
  end

  def after_action(%Ash.Query{} = subject, callback, _opts) do
    Ash.Query.after_action(subject, callback)
  end

  def after_action(subject, callback, opts) do
    if opts[:prepend?] do
      %{subject | after_action: [callback | subject.after_action]}
    else
      %{subject | after_action: subject.after_action ++ [callback]}
    end
  end

  @doc """
  Adds a before_transaction hook to the subject.

  Before transaction hooks are executed before the database transaction begins.
  They receive the subject and must return either a modified subject or an error tuple.
  These hooks are useful for validation, authorization checks, or preparatory logic
  that should run outside of the transaction.

  ## Parameters

    * `subject` - The subject to add the hook to (Changeset, Query, or ActionInput)
    * `callback` - Function that takes the subject and returns the subject or `{:error, reason}`
    * `opts` - Options including `:prepend?` to add at beginning of hooks list

  ## Examples

      # Add validation before transaction starts
      iex> changeset
      ...> |> Ash.Subject.before_transaction(fn changeset ->
      ...>   if valid_state?(changeset) do
      ...>     changeset
      ...>   else
      ...>     {:error, "Invalid state for this operation"}
      ...>   end
      ...> end)

      # Add logging for all subject types
      iex> subject
      ...> |> Ash.Subject.before_transaction(fn subject ->
      ...>   Logger.info("Starting transaction for \#{inspect(subject.resource)}")
      ...>   subject
      ...> end)

      # Prepend a hook to run first
      iex> query
      ...> |> Ash.Subject.before_transaction(check_permissions, prepend?: true)

  ## See also

  - `after_transaction/3` for hooks that run after the transaction completes
  - `around_transaction/3` for hooks that wrap the entire transaction
  - `before_action/3` for hooks that run before the action (inside transaction)
  """
  @spec before_transaction(
          subject :: t(),
          callback :: before_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def before_transaction(subject, callback, opts \\ [])

  def before_transaction(%Ash.Changeset{} = subject, callback, opts) do
    Ash.Changeset.before_transaction(subject, callback, opts)
  end

  def before_transaction(%Ash.Query{} = subject, callback, opts) do
    Ash.Query.before_transaction(subject, callback, opts)
  end

  def before_transaction(subject, callback, opts) do
    if opts[:prepend?] do
      %{subject | before_transaction: [callback | subject.before_transaction]}
    else
      %{subject | before_transaction: subject.before_transaction ++ [callback]}
    end
  end

  @doc """
  Adds an after_transaction hook to the subject.

  After transaction hooks are executed after the database transaction completes,
  regardless of success or failure. They receive both the subject and the transaction
  result, allowing for cleanup operations, logging, or result modification.

  ## Parameters

    * `subject` - The subject to add the hook to (Changeset, Query, or ActionInput)
    * `callback` - Function that takes the subject and result, returns modified result
    * `opts` - Options including `:prepend?` to add at beginning of hooks list

  ## Examples

      # Add cleanup after transaction
      iex> changeset
      ...> |> Ash.Subject.after_transaction(fn changeset, result ->
      ...>   cleanup_temp_resources()
      ...>   result
      ...> end)

      # Log transaction outcome
      iex> query
      ...> |> Ash.Subject.after_transaction(fn query, result ->
      ...>   case result do
      ...>     {:ok, _} -> Logger.info("Query succeeded")
      ...>     {:error, reason} -> Logger.error("Query failed: \#{inspect(reason)}")
      ...>   end
      ...>   result
      ...> end)

      # Modify successful results
      iex> action_input
      ...> |> Ash.Subject.after_transaction(fn input, result ->
      ...>   case result do
      ...>     {:ok, data} -> {:ok, Map.put(data, :processed_at, DateTime.utc_now())}
      ...>     error -> error
      ...>   end
      ...> end)

  ## Important Notes

  - These hooks run whether the transaction succeeds or fails
  - They run outside the transaction, so database operations here are not rolled back
  - The hook must return a result in the same format it received

  ## See also

  - `before_transaction/3` for hooks that run before the transaction starts
  - `around_transaction/3` for hooks that wrap the entire transaction
  - `after_action/3` for hooks that run after the action (inside transaction, success only)
  """
  @spec after_transaction(
          subject :: t(),
          callback :: after_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def after_transaction(subject, callback, opts \\ [])

  def after_transaction(%Ash.Changeset{} = subject, callback, opts) do
    Ash.Changeset.after_transaction(subject, callback, opts)
  end

  def after_transaction(%Ash.Query{} = subject, callback, opts) do
    Ash.Query.after_transaction(subject, callback, opts)
  end

  def after_transaction(subject, callback, opts) do
    if opts[:prepend?] do
      %{subject | after_transaction: [callback | subject.after_transaction]}
    else
      %{subject | after_transaction: subject.after_transaction ++ [callback]}
    end
  end

  @doc """
  Adds an around_transaction hook to the subject.

  Around transaction hooks wrap the entire transaction execution. They receive the subject
  and a callback function that executes the transaction. This allows adding logic both
  before and after the transaction while maintaining full control over its execution.

  ## Parameters

    * `subject` - The subject to add the hook to (Changeset, Query, or ActionInput)
    * `callback` - Function that takes the subject and a callback, must call the callback
    * `opts` - Options including `:prepend?` to add at beginning of hooks list

  ## Examples

      # Add timing measurements
      iex> changeset
      ...> |> Ash.Subject.around_transaction(fn changeset, callback ->
      ...>   start_time = System.monotonic_time(:millisecond)
      ...>   result = callback.(changeset)
      ...>   duration = System.monotonic_time(:millisecond) - start_time
      ...>   Logger.info("Transaction took \#{duration}ms")
      ...>   result
      ...> end)

      # Add retry logic for transient failures
      iex> query
      ...> |> Ash.Subject.around_transaction(fn query, callback ->
      ...>   case callback.(query) do
      ...>     {:error, %{retryable?: true}} = error ->
      ...>       Logger.warn("Retrying after error: \#{inspect(error)}")
      ...>       :timer.sleep(100)
      ...>       callback.(query)
      ...>     result ->
      ...>       result
      ...>   end
      ...> end)

      # Wrap with custom error handling
      iex> action_input
      ...> |> Ash.Subject.around_transaction(fn input, callback ->
      ...>   try do
      ...>     callback.(input)
      ...>   rescue
      ...>     exception ->
      ...>       Logger.error("Transaction failed: \#{Exception.message(exception)}")
      ...>       {:error, Exception.message(exception)}
      ...>   end
      ...> end)

  ## Warning

  This is an advanced hook that controls transaction execution. You **must** call the
  callback function provided to your hook, and the return value must match the structure
  returned by the callback (typically `{:ok, result}` or `{:error, reason}`).

  Failing to call the callback will prevent the transaction from executing at all.

  ## See also

  - `before_transaction/3` and `after_transaction/3` for simpler hooks
  - `around_action/2` for wrapping just the action execution
  """
  @spec around_transaction(
          subject :: t(),
          callback :: around_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def around_transaction(subject, callback, opts \\ [])

  def around_transaction(%Ash.Changeset{} = subject, callback, opts) do
    Ash.Changeset.around_transaction(subject, callback, opts)
  end

  def around_transaction(%Ash.Query{} = subject, callback, opts) do
    Ash.Query.around_transaction(subject, callback, opts)
  end

  def around_transaction(subject, callback, opts) do
    if opts[:prepend?] do
      %{subject | around_transaction: [callback | subject.around_transaction]}
    else
      %{subject | around_transaction: subject.around_transaction ++ [callback]}
    end
  end
end
