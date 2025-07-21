defmodule Ash.Subject do
  @moduledoc """
  Provides a consistent API for common operations across `Ash.Changeset`,
  `Ash.Query`, and `Ash.ActionInput`. It allows you to write generic code that works
  with any of these types without needing to pattern match or special-case your logic.
  """

  @type t :: Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t()

  @doc """
  Adds an error or list of errors to the subject.

  Supports all subject types (Changeset, Query, ActionInput) and maintains
  type consistency.

  ## Parameters

    * `subject` - The subject to add errors to
    * `errors` - Error or list of errors to add
  """
  @spec add_error(t(), Ash.Error.error_input() | list(Ash.Error.error_input())) :: t()
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
  @spec put_context(t(), atom, term()) :: t()
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
  @spec set_context(t(), map) :: t()
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
  @spec get_argument_or_attribute(t(), atom | binary) :: term()
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
  @spec get_argument(t(), atom | binary) :: term()
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

  @spec get_attribute(t(), atom) :: term()
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
  @spec fetch_argument(t(), atom | binary) :: {:ok, term()} | :error
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
  @spec set_arguments(t(), map) :: t()
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
  @spec set_argument(t(), atom | binary, term()) :: t()
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
  @spec delete_argument(t(), atom | binary | list(atom | binary)) :: t()
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
  @spec set_private_argument(Ash.Changeset.t() | Ash.ActionInput.t(), atom | binary, term()) ::
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
  @spec set_private_arguments(Ash.Changeset.t() | Ash.ActionInput.t(), map) ::
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
  @spec before_action(t(), (t() -> t()), Keyword.t()) :: t()
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
  @spec after_action(t(), (t(), term() -> {:ok, term()} | {:error, term()}), Keyword.t()) :: t()
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
  Executes all before_action callbacks on the subject.

  Only supported by Changeset and ActionInput.

  ## Parameters

    * `subject` - The subject to run callbacks on
  """
  @spec run_before_actions(Ash.Changeset.t() | Ash.ActionInput.t()) ::
          {Ash.Changeset.t() | Ash.ActionInput.t(), map()}
  def run_before_actions(%Ash.Changeset{} = subject) do
    Ash.Changeset.run_before_actions(subject)
  end

  def run_before_actions(%Ash.ActionInput{} = subject) do
    Ash.ActionInput.run_before_actions(subject)
  end

  @doc """
  Executes all after_action callbacks on the subject.

  Only supported by Changeset and ActionInput.

  ## Parameters

    * `result` - The result data from the action
    * `subject` - The subject that was executed
    * `notifications` - Notifications from before_action hooks
  """
  @spec run_after_actions(term(), Ash.Changeset.t() | Ash.ActionInput.t(), term()) ::
          {term(), list()}
  def run_after_actions(result, %Ash.Changeset{} = subject, before_action_notifications) do
    Ash.Changeset.run_after_actions(result, subject, before_action_notifications)
  end

  def run_after_actions(result, %Ash.ActionInput{} = subject, before_action_notifications) do
    Ash.ActionInput.run_after_actions(result, subject, before_action_notifications)
  end
end
