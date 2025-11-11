# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.ActionInput do
  @moduledoc """
  Input for a custom action

  Much like an `Ash.Query` and `Ash.Changeset` are used to provide inputs into
  CRUD actions, this struct provides the inputs required to execute a generic
  action.
  """

  alias Ash.Error.Action.InvalidArgument

  require Ash.Flags
  require Ash.Tracer

  defstruct [
    :action,
    :domain,
    :resource,
    :tenant,
    :to_tenant,
    invalid_keys: MapSet.new(),
    arguments: %{},
    params: %{},
    context: %{},
    valid?: true,
    errors: [],
    before_action: [],
    after_action: [],
    before_transaction: [],
    after_transaction: [],
    around_transaction: []
  ]

  @typedoc """
  Function type for before action hooks.

  Receives an action input and returns a modified action input, optionally with notifications.
  """
  @type before_action_fun :: (t -> t | {t, %{notifications: [Ash.Notifier.Notification.t()]}})

  @typedoc """
  Function type for after action hooks.

  Receives the action input and the result of the action, and can return
  the result optionally with notifications, or an error.
  """
  @type after_action_fun ::
          (t, term ->
             :ok
             | {:ok, [Ash.Notifier.Notification.t()]}
             | {:ok, term}
             | {:ok, term, [Ash.Notifier.Notification.t()]}
             | {:error, any})

  @typedoc """
  Function type for before transaction hooks.

  Receives an action input and returns a modified action input or an error.
  """
  @type before_transaction_fun :: (t -> t | {:error, any})

  @typedoc """
  Function type for after transaction hooks.

  Receives the action input and the result of the transaction, and returns
  the result (potentially modified) or an error.
  """
  @type after_transaction_fun ::
          (t, :ok | {:ok, term} | {:error, any} ->
             :ok | {:ok, term} | {:error, any})

  @typedoc """
  Function type for around transaction hooks.

  Receives an action input and a callback function that executes the transaction,
  and returns the result of calling the callback or an error.
  """
  @type around_transaction_fun ::
          (t, (t -> :ok | {:ok, term} | {:error, any}) ->
             :ok | {:ok, term} | {:error, any})

  @typedoc """
  An action input struct for generic (non-CRUD) actions.

  Contains all the information needed to execute a generic action including
  arguments, context, tenant information, validation state, and lifecycle hooks. Built using
  `for_action/4` and modified with functions like `set_argument/3` and `set_context/2`.
  """
  @type t :: %__MODULE__{
          arguments: map(),
          params: map(),
          tenant: term(),
          action: Ash.Resource.Actions.Action.t() | nil,
          resource: Ash.Resource.t(),
          invalid_keys: MapSet.t(),
          context: map(),
          domain: Ash.Domain.t() | nil,
          valid?: boolean(),
          errors: [Ash.Error.t()],
          before_action: [before_action_fun],
          after_action: [after_action_fun],
          before_transaction: [before_transaction_fun],
          after_transaction: [after_transaction_fun],
          around_transaction: [around_transaction_fun]
        }

  @doc """
  Creates a new action input from a resource.

  This creates a basic action input struct that can be used as a starting point
  for building inputs for generic actions. Use `for_action/4` to create an input
  bound to a specific action.

  ## Examples

      # Create a new action input for a resource
      iex> Ash.ActionInput.new(MyApp.Post)
      %Ash.ActionInput{resource: MyApp.Post, domain: nil, ...}


      # Usually you'll want to use for_action/4 instead
      iex> MyApp.Post |> Ash.ActionInput.for_action(:send_notification, %{message: "Hello"})
      %Ash.ActionInput{action: %{name: :send_notification}, arguments: %{message: "Hello"}, ...}

  ## See also

  - `for_action/4` for creating action-specific inputs
  - `set_argument/3` for adding arguments
  - `set_context/2` for adding context
  """
  @spec new(Ash.Resource.t(), Ash.Domain.t() | nil) :: t
  def new(resource, domain \\ nil) do
    %__MODULE__{resource: resource, domain: domain}
  end

  @for_action_opts [
    domain: [
      type: {:spark, Ash.Domain},
      doc: "The domain to use for the action. The resource's domain is used by default."
    ],
    context: [
      type: :map,
      doc: "Context to set on the action input.",
      default: %{}
    ],
    authorize?: [
      type: {:or, [:boolean, {:literal, nil}]},
      doc:
        "Whether or not to run authorization on the action. Default behavior of this option is controlled by the domain."
    ],
    tenant: [
      type: :any,
      doc: "The tenant to use for the action."
    ],
    scope: [
      type: :any,
      doc:
        "A value that implements the `Ash.Scope.ToOpts` protocol, for passing around actor/tenant/context in a single value. See `Ash.Scope.ToOpts` for more."
    ],
    actor: [
      type: :any,
      doc: "The actor performing the action"
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc: "A list of unknown inputs to skip. Use `:*` to skip all unknown inputs."
    ],
    tracer: [
      type: :any,
      doc: "A tracer or list of tracers to trace action execution."
    ],
    private_arguments: [
      type: :map,
      default: %{},
      doc: "A list of private arguments to be set before the action is invoked."
    ]
  ]

  for_action_opts = @for_action_opts

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: for_action_opts
  end

  @doc """
  Creates a new input for a generic action.

  This is the primary way to create action inputs for generic actions. It validates
  the action exists, sets up the input with proper defaults, and validates any
  provided arguments according to the action's argument definitions.

  ## Examples

      # Create input for a simple action
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{message: "Hello"})
      %Ash.ActionInput{action: %{name: :send_notification}, arguments: %{message: "Hello"}, ...}

      # Create input with options
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:complex_action, %{data: "test"},
      ...>   actor: current_user, authorize?: true)
      %Ash.ActionInput{arguments: %{data: "test"}, ...}

      # Create input and then modify it
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:example, %{})
      ...> |> Ash.ActionInput.set_context(%{source: "api"})
      iex> input.action.name
      :example

  ## Options

  #{Opts.docs()}

  ## See also

  - `new/2` for creating basic inputs
  - `set_argument/3` for adding arguments after creation
  - `Ash.run_action/2` for executing the action with the input
  - `d:Ash.Resource.Dsl.actions.action` for defining generic actions
  - [Generic Actions Guide](/documentation/topics/actions/generic-actions.md) for understanding generic actions
  - [Actions Guide](/documentation/topics/actions/actions.md) for general action concepts
  """
  @doc spark_opts: [{4, @for_action_opts}]
  @spec for_action(
          resource_or_input :: Ash.Resource.t() | t(),
          action :: atom,
          params :: map,
          opts :: Keyword.t()
        ) :: t()
  def for_action(resource_or_input, action, params, opts \\ []) do
    with {:ok, opts} <- Opts.validate(opts),
         {:ok, input} <- set_action_for_input(resource_or_input, action),
         opts <- Opts.to_options(opts),
         {:ok, input} <- set_domain_for_input(input, opts) do
      {input, _opts} = Ash.Actions.Helpers.set_context_and_get_opts(input.domain, input, opts)

      input =
        Enum.reduce(opts[:private_arguments] || %{}, input, fn {k, v}, input ->
          Ash.ActionInput.set_private_argument(input, k, v)
        end)

      input
      |> cast_params(params, opts)
      |> set_defaults()
      |> require_arguments()
      |> run_preparations_and_validations(opts)
    else
      {:error, reason} -> raise Ash.Error.to_error_class(reason)
    end
  end

  defp set_action_for_input(%Ash.ActionInput{resource: resource} = input, action_name)
       when is_atom(resource) do
    case Ash.Resource.Info.action(resource, action_name) do
      nil ->
        {:error,
         Ash.Error.Invalid.NoSuchAction.exception(
           resource: resource,
           action: action_name,
           type: :action
         )}

      action when action.type == :action ->
        {:ok, %{input | action: action}}

      action ->
        {:error,
         Ash.Error.Invalid.NoSuchAction.exception(
           resource: resource,
           action: action,
           type: :action
         )}
    end
  end

  defp set_action_for_input(resource, action) when is_atom(resource) do
    resource
    |> new()
    |> set_action_for_input(action)
  end

  defp set_domain_for_input(input, opts) do
    domain =
      input.domain || opts[:domain] || Ash.Resource.Info.domain(input.resource) ||
        Ash.Actions.Helpers.maybe_embedded_domain(input.resource)

    if domain do
      {:ok, %{input | domain: domain}}
    else
      {:error,
       ArgumentError.exception(
         "Could not determine domain for action. Provide the `domain` option or configure a domain in the resource directly."
       )}
    end
  end

  @doc """
  Sets the tenant to use when calling the action.

  In multitenant applications, this configures which tenant's data the action
  should operate on. The tenant value is used for data isolation and access control.

  ## Examples

      # Set tenant using a string identifier
      iex> MyApp.Post
      ...> |> Ash.ActionInput.new()
      ...> |> Ash.ActionInput.set_tenant("org_123")
      ...> |> then(& &1.tenant)
      "org_123"

      # Set tenant using a struct that implements Ash.ToTenant
      iex> org = %MyApp.Organization{id: 456}
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{})
      ...> |> Ash.ActionInput.set_tenant(org)
      iex> input.tenant == org
      true

      # Use with action execution
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:cleanup, %{})
      ...> |> Ash.ActionInput.set_tenant("tenant_456")
      ...> |> Ash.run_action()

  ## See also

  - `for_action/4` for setting tenant when creating inputs
  - `Ash.ToTenant` protocol for custom tenant conversion
  - `set_context/2` for adding tenant to action context
  """
  @spec set_tenant(t(), Ash.ToTenant.t()) :: t()
  def set_tenant(input, tenant) do
    %{input | tenant: tenant, to_tenant: Ash.ToTenant.to_tenant(tenant, input.resource)}
  end

  defp require_arguments(input) do
    input.action.arguments
    |> Enum.filter(&(&1.allow_nil? == false))
    |> Enum.reduce(input, fn argument, input ->
      case fetch_argument(input, argument.name) do
        {:ok, value} when not is_nil(value) ->
          input

        _ ->
          if argument.name in input.invalid_keys do
            input
          else
            add_error(
              input,
              Ash.Error.Changes.Required.exception(
                resource: input.resource,
                field: argument.name,
                type: :argument
              )
            )
          end
      end
    end)
  end

  defp set_defaults(input) do
    input.action.arguments
    |> Enum.reject(&is_nil(&1.default))
    |> Enum.reduce(input, fn argument, input ->
      case fetch_argument(input, argument.name) do
        {:ok, value} when not is_nil(value) ->
          input

        _ ->
          if argument.name in input.invalid_keys do
            input
          else
            set_argument(input, argument.name, default(argument))
          end
      end
    end)
  end

  defp default(%{default: {mod, func, args}}), do: apply(mod, func, args)
  defp default(%{default: function}) when is_function(function, 0), do: function.()
  defp default(%{default: value}), do: value

  @doc """
  Gets the value of an argument provided to the input.

  Returns the argument value if found, or `nil` if not found. Arguments are
  validated and cast according to the action's argument definitions when set.

  ## Examples

      # Get an argument that exists
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_email, %{recipient: "user@example.com"})
      ...> |> Ash.ActionInput.get_argument(:recipient)
      "user@example.com"

      # Get an argument that doesn't exist returns nil
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_email, %{})
      ...> |> Ash.ActionInput.get_argument(:missing_arg)
      nil

      # Arguments can be accessed by string or atom key
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:example, %{"message" => "hello"})
      ...> |> Ash.ActionInput.get_argument(:message)
      "hello"

  ## See also

  - `fetch_argument/2` for safer argument access with explicit error handling
  - `set_argument/3` for setting argument values
  - `for_action/4` for providing initial arguments
  """
  @spec get_argument(t, atom | String.t()) :: term
  def get_argument(input, argument) when is_atom(argument) or is_binary(argument) do
    case fetch_argument(input, argument) do
      {:ok, value} -> value
      :error -> nil
    end
  end

  @doc """
  Fetches the value of an argument provided to the input.

  Returns `{:ok, value}` if the argument exists, or `:error` if not found.
  This is the safer alternative to `get_argument/2` when you need to distinguish
  between a `nil` value and a missing argument.

  ## Examples

      # Fetch an argument that exists
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{priority: :high})
      ...> |> Ash.ActionInput.fetch_argument(:priority)
      {:ok, :high}

      # Fetch an argument that doesn't exist
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{})
      ...> |> Ash.ActionInput.fetch_argument(:missing_arg)
      :error

      # Distinguish between nil and missing arguments
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:example, %{optional_field: nil})
      ...> |> Ash.ActionInput.fetch_argument(:optional_field)
      {:ok, nil}

      # Use in conditional logic
      iex> input = MyApp.Post |> Ash.ActionInput.for_action(:process, %{})
      iex> case Ash.ActionInput.fetch_argument(input, :mode) do
      ...>   {:ok, mode} -> "Processing in \#{mode} mode"
      ...>   :error -> "Using default processing mode"
      ...> end
      "Using default processing mode"

  ## See also

  - `get_argument/2` for simpler argument access
  - `set_argument/3` for setting argument values
  - `for_action/4` for providing initial arguments
  """
  @spec fetch_argument(t, atom | String.t()) :: {:ok, term()} | :error
  def fetch_argument(input, argument) when is_atom(argument) or is_binary(argument) do
    with :error <- Map.fetch(input.arguments, argument) do
      Map.fetch(input.arguments, to_string(argument))
    end
  end

  @doc """
  Sets an argument value on the action input.

  The argument value is validated and cast according to the action's argument
  definition. If validation fails, errors will be added to the input and it
  will be marked as invalid.

  ## Examples

      # Set a simple argument
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{})
      ...> |> Ash.ActionInput.set_argument(:message, "Hello World")
      ...> |> Ash.ActionInput.get_argument(:message)
      "Hello World"

      # Set multiple arguments by chaining
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:complex_action, %{})
      ...> |> Ash.ActionInput.set_argument(:priority, :high)
      ...> |> Ash.ActionInput.set_argument(:batch_size, 100)
      iex> Ash.ActionInput.get_argument(input, :priority)
      :high

      # Arguments are validated according to type
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:schedule_job, %{})
      ...> |> Ash.ActionInput.set_argument(:run_at, ~U[2024-01-01 10:00:00Z])
      iex> input.valid?
      true

  ## See also

  - `get_argument/2` for retrieving argument values
  - `set_private_argument/3` for setting private arguments
  - `for_action/4` for providing initial arguments
  """
  @spec set_argument(input :: t(), name :: atom | String.t(), value :: term()) :: t()
  def set_argument(input, argument, value) do
    if input.action do
      argument =
        Enum.find(
          input.action.arguments,
          &(&1.name == argument || to_string(&1.name) == argument)
        )

      if argument do
        with value <- Ash.Type.Helpers.handle_indexed_maps(argument.type, value),
             constraints <- Ash.Type.include_source(argument.type, input, argument.constraints),
             {:ok, casted} <-
               Ash.Type.cast_input(argument.type, value, constraints),
             {:constrained, {:ok, casted}, argument} when not is_nil(casted) <-
               {:constrained, Ash.Type.apply_constraints(argument.type, casted, constraints),
                argument} do
          %{input | arguments: Map.put(input.arguments, argument.name, casted)}
        else
          {:constrained, {:ok, nil}, _argument} ->
            %{input | arguments: Map.put(input.arguments, argument.name, nil)}

          {:constrained, {:error, error}, argument} ->
            input = %{
              input
              | arguments: Map.put(input.arguments, argument.name, value)
            }

            add_invalid_errors(value, input, argument, error)

          {:error, error} ->
            input = %{
              input
              | arguments: Map.put(input.arguments, argument.name, value)
            }

            add_invalid_errors(value, input, argument, error)
        end
      else
        input
      end
    else
      input
    end
  end

  @doc """
  Deletes one or more arguments from the subject.

  ## Parameters

    * `subject` - The subject to delete arguments from
    * `arguments` - Single argument name or list of argument names to delete
  """
  @spec delete_argument(
          input :: t(),
          argument_or_arguments :: atom | String.t() | list(atom | String.t())
        ) :: t()
  def delete_argument(input, argument_or_arguments) do
    argument_or_arguments
    |> List.wrap()
    |> Enum.reduce(input, fn argument, input ->
      %{input | arguments: Map.delete(input.arguments, argument)}
    end)
  end

  @doc """
  Sets a private argument value on the action input.

  Private arguments are not exposed in the public API and can only be set
  internally. This function will only work for arguments marked as `public?: false`
  in the action definition.

  ## Examples

      # Set a private argument (assuming :internal_flag is private)
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:example, %{})
      ...> |> Ash.ActionInput.set_private_argument(:internal_flag, true)
      ...> |> Ash.ActionInput.get_argument(:internal_flag)
      true

      # Attempting to set a public argument as private will error
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:example, %{})
      ...> |> Ash.ActionInput.set_private_argument(:public_arg, "value")
      iex> input.valid?
      false

      # Use in action implementations for internal state
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:complex_workflow, %{data: "user_data"})
      ...> |> Ash.ActionInput.set_private_argument(:workflow_step, 1)

  ## See also

  - `set_argument/3` for setting public arguments
  - `get_argument/2` for retrieving argument values
  - Action argument definitions with `public?: false`
  """
  @spec set_private_argument(input :: t(), name :: atom, value :: term()) :: t()
  def set_private_argument(input, name, value) do
    argument =
      Enum.find(
        input.action.arguments,
        &(&1.name == name || to_string(&1.name) == name)
      )

    cond do
      is_nil(argument) ->
        input

      argument.public? ->
        add_invalid_errors(
          value,
          input,
          argument,
          "can't set public arguments with set_private_argument/3"
        )

      true ->
        set_argument(input, name, value)
    end
  end

  @doc """
  Deep merges the provided map into the input context.

  Context is used to pass additional information through the action pipeline
  that can be accessed by action implementations, changes, and validations.
  The context is merged deeply, so nested maps will be combined rather than replaced.

  Do not use the `private` key in your custom context, as that is reserved for
  internal use.

  ## Examples

      # Set simple context values
      iex> MyApp.Post
      ...> |> Ash.ActionInput.new()
      ...> |> Ash.ActionInput.set_context(%{source: "api", user_id: 123})
      ...> |> then(& &1.context.source)
      "api"

      # Context is merged deeply
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.new()
      ...> |> Ash.ActionInput.set_context(%{metadata: %{version: 1}})
      ...> |> Ash.ActionInput.set_context(%{metadata: %{trace_id: "abc123"}})
      iex> input.context.metadata
      %{version: 1, trace_id: "abc123"}

      # Use context in action implementations
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:process_data, %{data: "test"})
      ...> |> Ash.ActionInput.set_context(%{
      ...>   request_id: "req_456",
      ...>   feature_flags: %{new_algorithm: true}
      ...> })

  ## See also

  - `for_action/4` for setting context when creating inputs
  - Action implementations can access context for custom logic
  - `set_tenant/2` for tenant-specific context
  """
  @spec set_context(t(), map | nil) :: t()
  def set_context(input, nil), do: input

  def set_context(input, map) do
    %{
      input
      | context:
          input.context
          |> Ash.Helpers.deep_merge_maps(map)
          |> then(&Ash.Helpers.deep_merge_maps(&1, map[:shared] || %{}))
    }
  end

  defp cast_params(input, params, opts) do
    input = %{
      input
      | params: Map.merge(input.params, Enum.into(params, %{}))
    }

    skip_unknown_inputs =
      List.wrap(opts[:skip_unknown_inputs] || input.action.skip_unknown_inputs)

    Enum.reduce(params, input, fn {name, value}, input ->
      cond do
        has_argument?(input.action, name) ->
          set_argument(input, name, value)

        match?("_" <> _, name) ->
          input

        :* in skip_unknown_inputs ->
          input

        name in skip_unknown_inputs ->
          input

        true ->
          error =
            Ash.Error.Invalid.NoSuchInput.exception(
              resource: input.resource,
              action: input.action.name,
              input: name,
              inputs: Enum.map(input.action.arguments, & &1.name)
            )

          add_error(input, Ash.Error.set_path(error, name))
      end
    end)
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.public? && &1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(&1.public? && to_string(&1.name) == name))
  end

  defp add_invalid_errors(value, input, attribute, message) do
    input = %{input | invalid_keys: MapSet.put(input.invalid_keys, attribute.name)}

    messages =
      if Keyword.keyword?(message) do
        [message]
      else
        List.wrap(message)
      end

    Enum.reduce(messages, input, fn message, input ->
      if is_exception(message) do
        error =
          message
          |> Ash.Error.to_ash_error()

        errors =
          case error do
            %class{errors: errors}
            when class in [
                   Ash.Error.Invalid,
                   Ash.Error.Unknown,
                   Ash.Error.Forbidden,
                   Ash.Error.Framework
                 ] ->
              errors

            error ->
              [error]
          end

        Enum.reduce(errors, input, fn error, input ->
          add_error(input, Ash.Error.set_path(error, attribute.name))
        end)
      else
        opts = Ash.Type.Helpers.error_to_exception_opts(message, attribute)

        Enum.reduce(opts, input, fn opts, input ->
          error =
            InvalidArgument.exception(
              value: value,
              field: Keyword.get(opts, :field),
              message: Keyword.get(opts, :message),
              vars: opts
            )

          error =
            if opts[:path] do
              Ash.Error.set_path(error, opts[:path])
            else
              error
            end

          add_error(input, error)
        end)
      end
    end)
  end

  @doc """
  Adds an error to the errors list and marks the action input as invalid.

  This function allows you to add validation errors or other issues to the
  action input. Once an error is added, the input will be marked as invalid
  and action execution will be prevented.

  ## Examples

      # Add a simple string error
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{})
      ...> |> Ash.ActionInput.add_error("Missing required configuration")
      iex> input.valid?
      false

      # Add an error with a specific path
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:process_data, %{})
      ...> |> Ash.ActionInput.add_error("Invalid format", [:data, :format])
      iex> input.errors |> List.first() |> Map.get(:path)
      [:data, :format]

      # Add multiple errors
      iex> input = MyApp.Post
      ...> |> Ash.ActionInput.for_action(:complex_action, %{})
      ...> |> Ash.ActionInput.add_error(["Error 1", "Error 2"])
      iex> length(input.errors)
      2

      # Add structured error with keyword list
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:validate_input, %{})
      ...> |> Ash.ActionInput.add_error(field: :email, message: "is invalid")

  ## See also

  - `Ash.Error.to_ash_error/3` for more on supported error values
  - Action implementations can use this to add custom validation errors
  - `set_argument/3` automatically adds errors for invalid argument values
  """
  @spec add_error(
          t(),
          Ash.Error.error_input() | list(Ash.Error.error_input()),
          Ash.Error.path_input()
        ) :: t()
  @spec add_error(t(), Ash.Error.error_input() | list(Ash.Error.error_input())) :: t()
  def add_error(input, errors, path \\ [])

  def add_error(input, [], _path) do
    input
  end

  def add_error(input, errors, path) when is_list(errors) do
    if Keyword.keyword?(errors) do
      errors
      |> to_change_errors()
      |> Ash.Error.set_path(path)
      |> handle_error(input)
    else
      Enum.reduce(errors, input, &add_error(&2, &1, path))
    end
  end

  def add_error(input, error, path) when is_binary(error) do
    add_error(
      input,
      InvalidArgument.exception(message: error),
      path
    )
  end

  def add_error(input, error, path) do
    error
    |> Ash.Error.set_path(path)
    |> handle_error(input)
  end

  defp handle_error(error, input) do
    %{input | valid?: false, errors: [error | input.errors]}
  end

  defp to_change_errors(keyword) do
    errors =
      if keyword[:fields] && keyword[:fields] != [] do
        Enum.map(keyword[:fields], fn field ->
          InvalidArgument.exception(
            field: field,
            message: keyword[:message],
            value: keyword[:value],
            vars: keyword
          )
        end)
      else
        InvalidArgument.exception(
          field: keyword[:field],
          message: keyword[:message],
          value: keyword[:value],
          vars: keyword
        )
      end

    if keyword[:path] do
      Enum.map(errors, &Ash.Error.set_path(&1, keyword[:path]))
    else
      errors
    end
  end

  @doc """
  Adds a before_action hook to the action input.

  Before action hooks are called with the action input and can modify it before
  the action executes. They can also add errors to halt processing or return
  notifications to be processed later.

  ## Examples

      # Validate arguments before action
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:send_notification, %{message: "Hello"})
      ...> |> Ash.ActionInput.before_action(fn input ->
      ...>   if String.length(input.arguments.message) > 100 do
      ...>     Ash.ActionInput.add_error(input, "Message too long")
      ...>   else
      ...>     input
      ...>   end
      ...> end)

      # Set computed arguments
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:process_data, %{data: "test"})
      ...> |> Ash.ActionInput.before_action(fn input ->
      ...>   Ash.ActionInput.set_argument(input, :processed_at, DateTime.utc_now())
      ...> end)

      # Return notifications
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:audit_action, %{})
      ...> |> Ash.ActionInput.before_action(fn input ->
      ...>   notification = %Ash.Notifier.Notification{
      ...>     resource: input.resource,
      ...>     action: input.action,
      ...>     data: %{audit: "before_action"}
      ...>   }
      ...>   {input, %{notifications: [notification]}}
      ...> end)

  ## Options

  - `prepend?` - If `true`, adds the hook to the beginning of the list instead of the end

  ## See also

  - `after_action/2` for hooks that run after the action completes
  - `for_action/4` for creating action inputs
  - `add_error/2` for adding validation errors in hooks
  """
  @spec before_action(
          input :: t(),
          fun :: before_action_fun(),
          opts :: Keyword.t()
        ) ::
          t()
  def before_action(input, func, opts \\ []) do
    if opts[:prepend?] do
      %{input | before_action: [func | input.before_action]}
    else
      %{input | before_action: input.before_action ++ [func]}
    end
  end

  @doc """
  Adds an after_action hook to the action input.

  After action hooks are called with the action input and the result returned
  from the action. They can modify the result, perform side effects, or return
  errors to halt processing. The hook can return notifications alongside the result.

  > #### Actions without a return type {: .tip}
  >
  > After action hooks will work for generic actions without a return type,
  > however they will receive `nil` as their `result` argument and are
  > expected to return
  > `:ok | {:ok, [Ash.Notifier.Notification.t()]} | {:error, term}`.

  ## Examples

      # Transform the result after action
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:calculate_stats, %{data: [1, 2, 3]})
      ...> |> Ash.ActionInput.after_action(fn input, result ->
      ...>   enhanced_result = Map.put(result, :calculated_at, DateTime.utc_now())
      ...>   {:ok, enhanced_result}
      ...> end)

      # Log successful actions
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:important_action, %{})
      ...> |> Ash.ActionInput.after_action(fn inp, result ->
      ...>   Logger.info("Action completed successfully")
      ...>   {:ok, result}
      ...> end)

      # Return notifications
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:notify_users, %{message: "Hello"})
      ...> |> Ash.ActionInput.after_action(fn input, result ->
      ...>   notification = %Ash.Notifier.Notification{
      ...>     resource: input.resource,
      ...>     action: input.action,
      ...>     data: result
      ...>   }
      ...>   {:ok, result, [notification]}
      ...> end)

      # Handle errors
      iex> MyApp.Post
      ...> |> Ash.ActionInput.for_action(:risky_action, %{})
      ...> |> Ash.ActionInput.after_action(fn input, result ->
      ...>   if is_error_result?(result) do
      ...>     {:error, "Action failed with custom error"}
      ...>   else
      ...>     {:ok, result}
      ...>   end
      ...> end)

  ## See also

  - `before_action/3` for hooks that run before the action executes
  - `for_action/4` for creating action inputs
  - `Ash.run_action/2` for executing the action with the input
  """
  @spec after_action(
          input :: t(),
          fun :: after_action_fun(),
          opts :: Keyword.t()
        ) :: t()
  def after_action(input, func, opts \\ []) do
    if opts[:prepend?] do
      %{input | after_action: [func | input.after_action]}
    else
      %{input | after_action: input.after_action ++ [func]}
    end
  end

  @doc """
  Adds a before transaction hook to the action input.

  Before transaction hooks are executed before the transaction begins (if the action is transactional).
  They can modify the action input or halt execution by returning an error.

  ## Examples

      # Add logging before transaction
      iex> input
      ...> |> Ash.ActionInput.before_transaction(fn input ->
      ...>   IO.puts("Starting transaction for action")
      ...>   input
      ...> end)

  ## See also

  - `after_transaction/2` for hooks that run after the transaction
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `before_action/3` for hooks that run before the action (inside transaction)
  """
  @spec before_transaction(
          input :: t(),
          fun :: before_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def before_transaction(input, func, opts \\ []) do
    if opts[:prepend?] do
      %{input | before_transaction: [func | input.before_transaction]}
    else
      %{input | before_transaction: input.before_transaction ++ [func]}
    end
  end

  @doc """
  Adds an after transaction hook to the action input.

  After transaction hooks are executed after the transaction completes, regardless of success or failure.
  They receive both the input and the transaction result, and can modify the result.

  > #### Actions without a return type {: .tip}
  >
  > After transaction hooks will work for generic actions without a return
  > type, however they will receive `:ok | {:error, term}` as their `result`
  > argument and are expected to return `:ok | {:error, term}`.

  ## Examples

      # Add cleanup after transaction
      iex> input
      ...> |> Ash.ActionInput.after_transaction(fn input, result ->
      ...>   cleanup_resources()
      ...>   result
      ...> end)

  ## See also

  - `before_transaction/2` for hooks that run before the transaction
  - `around_transaction/2` for hooks that wrap the entire transaction
  - `after_action/2` for hooks that run after the action (inside transaction)
  """
  @spec after_transaction(
          input :: t(),
          fun :: after_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def after_transaction(input, func, opts \\ []) do
    if opts[:prepend?] do
      %{input | after_transaction: [func | input.after_transaction]}
    else
      %{input | after_transaction: input.after_transaction ++ [func]}
    end
  end

  @doc """
  Adds an around transaction hook to the action input.

  Around transaction hooks wrap the entire transaction execution. They receive a callback
  function that they must call to execute the transaction, allowing them to add logic
  both before and after the transaction.

  > #### Actions without a return type {: .tip}
  >
  > Around transaction hooks will work for generic actions without a return
  > type, however they will receive `:ok` as the result of the callback
  > instead of `{:ok, result}`. They are expected to return
  > `:ok | {:error, any}`.

  ## Examples

      # Add retry logic around transaction
      iex> input
      ...> |> Ash.ActionInput.around_transaction(fn input, callback ->
      ...>   case callback.(input) do
      ...>     {:ok, result} -> {:ok, result}
      ...>     {:error, %{retryable?: true}} -> callback.(input) # Retry once
      ...>     error -> error
      ...>   end
      ...> end)

  ## See also

  - `before_transaction/2` for hooks that run before the transaction
  - `after_transaction/2` for hooks that run after the transaction
  - `before_action/3` and `after_action/2` for hooks that run inside the transaction
  """
  @spec around_transaction(
          input :: t(),
          fun :: around_transaction_fun(),
          opts :: Keyword.t()
        ) :: t()
  def around_transaction(input, func, opts \\ []) do
    if opts[:prepend?] do
      %{input | around_transaction: [func | input.around_transaction]}
    else
      %{input | around_transaction: input.around_transaction ++ [func]}
    end
  end

  defguardp has_return?(input) when not is_nil(input.action.returns)
  defguardp has_no_return?(input) when is_nil(input.action.returns)

  defp run_preparations_and_validations(input, opts) do
    actor = opts[:actor]
    authorize? = opts[:authorize?]
    tracer = opts[:tracer]

    metadata = fn ->
      %{
        domain: input.domain,
        resource: input.resource,
        resource_short_name: Ash.Resource.Info.short_name(input.resource),
        actor: actor,
        tenant: input.tenant,
        action: input.action && input.action.name,
        authorize?: authorize?
      }
    end

    # Get global validations unless skipped
    global_validations =
      if input.action.skip_global_validations? do
        []
      else
        input.resource
        |> Ash.Resource.Info.validations()
        |> Enum.filter(&(:action in &1.on))
      end

    # Run preparations and validations
    input.resource
    # 4.0 run global preparations after
    # action preparations
    |> Ash.Resource.Info.preparations(:action)
    |> Enum.concat(input.action.preparations || [])
    |> Enum.concat(global_validations)
    |> Enum.reduce(input, fn
      %{only_when_valid?: true}, %{valid?: false} = input ->
        input

      %{validation: {module, opts}} = validation, input ->
        if __MODULE__ not in module.supports(opts) do
          raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
        end

        validate(input, validation, tracer, metadata, actor)

      %{preparation: {module, opts}} = preparation, input ->
        if __MODULE__ not in module.supports(opts) do
          raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
        end

        run_preparation(preparation, input, actor, authorize?, tracer, metadata)
    end)
  end

  defp validate(input, validation, tracer, metadata, actor) do
    if validation.before_action? do
      before_action(input, fn input ->
        if validation.only_when_valid? and not input.valid? do
          input
        else
          do_validation(input, validation, tracer, metadata, actor)
        end
      end)
    else
      if validation.only_when_valid? and not input.valid? do
        input
      else
        do_validation(input, validation, tracer, metadata, actor)
      end
    end
  end

  defp do_validation(input, validation, tracer, metadata, actor) do
    context = %{
      actor: input.context[:private][:actor],
      tenant: input.tenant,
      source_context: input.context,
      authorize?: input.context[:private][:authorize?] || false,
      tracer: input.context[:private][:tracer]
    }

    if Enum.all?(validation.where || [], fn {module, opts} ->
         if __MODULE__ not in module.supports(opts) do
           raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
         end

         opts =
           Ash.Expr.fill_template(
             opts,
             actor: actor,
             tenant: input.to_tenant,
             args: input.arguments,
             context: input.context
           )

         case module.init(opts) do
           {:ok, opts} ->
             Ash.Resource.Validation.validate(
               module,
               input,
               opts,
               struct(Ash.Resource.Validation.Context, context)
             ) ==
               :ok

           _ ->
             false
         end
       end) do
      Ash.Tracer.span :validation, fn -> "validate: #{inspect(validation.module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :validation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            validation: inspect(validation.module)
          }
        end do
          Ash.Tracer.set_metadata(tracer, :validation, metadata)

          opts =
            Ash.Expr.fill_template(
              validation.opts,
              actor: actor,
              tenant: input.to_tenant,
              args: input.arguments,
              context: input.context
            )

          with {:ok, opts} <- validation.module.init(opts),
               :ok <-
                 Ash.Resource.Validation.validate(
                   validation.module,
                   input,
                   opts,
                   struct(Ash.Resource.Validation.Context, context)
                 ) do
            input
          else
            {:error, error} ->
              add_error(input, error)

            :error ->
              add_error(input, validation.module.describe(validation.opts))
          end
        end
      end
    else
      input
    end
  end

  defp run_preparation(
         %{preparation: {module, opts}} = preparation,
         input,
         actor,
         authorize?,
         tracer,
         metadata
       ) do
    context = %{
      actor: actor,
      tenant: input.tenant,
      source_context: input.context,
      authorize?: authorize? || false,
      tracer: tracer
    }

    if Enum.all?(preparation.where || [], fn {module, opts} ->
         if __MODULE__ not in module.supports(opts) do
           raise Ash.Error.Framework.UnsupportedSubject, subject: __MODULE__, module: module
         end

         opts =
           Ash.Expr.fill_template(
             opts,
             actor: actor,
             tenant: input.to_tenant,
             args: input.arguments,
             context: input.context
           )

         case module.init(opts) do
           {:ok, opts} ->
             Ash.Resource.Validation.validate(
               module,
               input,
               opts,
               struct(Ash.Resource.Validation.Context, context)
             ) ==
               :ok

           _ ->
             false
         end
       end) do
      Ash.Tracer.span :preparation, fn -> "prepare: #{inspect(module)}" end, tracer do
        Ash.Tracer.telemetry_span [:ash, :preparation], fn ->
          %{
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            preparation: inspect(module)
          }
        end do
          Ash.Tracer.set_metadata(tracer, :preparation, metadata)

          {:ok, opts} = module.init(opts)

          opts =
            Ash.Expr.fill_template(
              opts,
              actor: actor,
              tenant: input.to_tenant,
              args: input.arguments,
              context: input.context
            )

          preparation_context = struct(Ash.Resource.Preparation.Context, context)
          Ash.Resource.Preparation.prepare(module, input, opts, preparation_context)
        end
      end
    else
      input
    end
  end

  @doc false
  def run_before_actions(%{before_action: []} = input), do: {input, %{notifications: []}}

  def run_before_actions(%{valid?: false} = input), do: {input, %{notifications: []}}

  def run_before_actions(input) do
    Enum.reduce_while(
      input.before_action,
      {input, %{notifications: []}},
      fn before_action, {input, instructions} ->
        metadata = fn ->
          %{
            domain: input.domain,
            resource: input.resource,
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            actor: input.context[:private][:actor],
            tenant: input.context[:private][:tenant],
            action: input.action && input.action.name,
            authorize?: input.context[:private][:authorize?]
          }
        end

        tracer = input.context[:private][:tracer]

        result =
          Ash.Tracer.span :before_action,
                          "before_action",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :before_action, metadata)

            Ash.Tracer.telemetry_span [:ash, :before_action], metadata do
              before_action.(input)
            end
          end

        case result do
          {:error, error} ->
            {:halt, {:error, error}}

          {input, %{notifications: notifications}} ->
            cont =
              if input.valid? do
                :cont
              else
                :halt
              end

            {cont,
             {input,
              %{
                instructions
                | notifications: List.wrap(instructions.notifications) ++ List.wrap(notifications)
              }}}

          %Ash.ActionInput{} = input ->
            cont =
              if input.valid? do
                :cont
              else
                :halt
              end

            {cont, {input, instructions}}

          other ->
            raise """
            Invalid return value from before_action hook. Expected one of:

            * %Ash.ActionInput{}
            * {%Ash.ActionInput{}, %{notifications: [...]}}
            * {:error, error}

            Got:

            #{inspect(other)}
            """
        end
      end
    )
    |> case do
      {:error, error} ->
        {:error, error}

      {%{valid?: true} = input, instructions} ->
        {input, instructions}

      {%{valid?: false} = input, _instructions} ->
        {:error, Ash.Error.to_error_class(input.errors)}
    end
  end

  @doc false
  def run_after_actions(result, input, before_action_notifications) do
    Enum.reduce_while(
      input.after_action,
      {:ok, result, input, %{notifications: before_action_notifications}},
      fn after_action, {:ok, result, input, acc} ->
        tracer = input.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: input.domain,
            resource: input.resource,
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            actor: input.context[:private][:actor],
            tenant: input.context[:private][:tenant],
            action: input.action && input.action.name,
            authorize?: input.context[:private][:authorize?]
          }
        end

        result =
          Ash.Tracer.span :after_action,
                          "after_action",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :after_action, metadata)

            Ash.Tracer.telemetry_span [:ash, :after_action], metadata do
              after_action.(input, result)
            end
          end

        case result do
          {:ok, new_result, new_notifications} when has_return?(input) ->
            {:cont,
             {:ok, new_result, input,
              %{
                acc
                | notifications:
                    merge_after_action_notifications(
                      input,
                      new_result,
                      acc.notifications,
                      new_notifications
                    )
              }}}

          :ok when has_no_return?(input) ->
            {:cont, {:ok, nil, input, acc}}

          {:ok, new_notifications} when has_no_return?(input) and is_list(new_notifications) ->
            {:cont,
             {:ok, nil, input,
              %{
                acc
                | notifications:
                    merge_after_action_notifications(
                      input,
                      nil,
                      acc.notifications,
                      new_notifications
                    )
              }}}

          {:ok, new_result} when has_return?(input) ->
            {:cont, {:ok, new_result, input, acc}}

          {:error, error} ->
            {:halt, {:error, error}}

          other when has_return?(input) ->
            raise """
            Invalid return value from after_action hook. Because this action has a return type I expected one of:

            * {:ok, result}
            * {:ok, result, notifications}
            * {:error, error}

            Got:

            #{inspect(other)}
            """

          other when has_no_return?(input) ->
            raise """
            Invalid return value from after_action hook. Because this action has no return type I expected one of:

            * :ok
            * {:ok, notifications}
            * {:error, error}

            Got:

            #{inspect(other)}
            """
        end
      end
    )
  end

  defp merge_after_action_notifications(input, result, old_notifications, new_notifications) do
    Enum.map(
      List.wrap(old_notifications) ++ List.wrap(new_notifications),
      fn notification ->
        %{
          notification
          | resource: notification.resource || input.resource,
            action:
              notification.action ||
                Ash.Resource.Info.action(
                  input.resource,
                  input.action.name,
                  :action
                ),
            data: notification.data || result,
            actor: notification.actor || input.context[:private][:actor]
        }
      end
    )
  end

  @doc false
  def run_before_transaction_hooks(%{before_transaction: []} = input) do
    {:ok, input}
  end

  def run_before_transaction_hooks(input) do
    Enum.reduce_while(
      input.before_transaction,
      {:ok, input},
      fn before_transaction, {:ok, input} ->
        tracer = input.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: input.domain,
            resource: input.resource,
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            actor: input.context[:private][:actor],
            tenant: input.context[:private][:tenant],
            action: input.action && input.action.name,
            authorize?: input.context[:private][:authorize?]
          }
        end

        result =
          Ash.Tracer.span :before_transaction,
                          "before_transaction",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :before_transaction, metadata)

            Ash.Tracer.telemetry_span [:ash, :before_transaction], metadata do
              before_transaction.(input)
            end
          end

        case result do
          {:error, error} ->
            {:halt, {:error, error}}

          %Ash.ActionInput{} = input ->
            cont =
              if input.valid? do
                :cont
              else
                :halt
              end

            {cont, {:ok, input}}

          other ->
            raise """
            Invalid return value from before_transaction hook. Expected one of:

            * %Ash.ActionInput{}
            * {:error, error}

            Got:

            #{inspect(other)}
            """
        end
      end
    )
  end

  @doc false
  def run_after_transaction_hooks(result, %{after_transaction: []} = _input) do
    result
  end

  def run_after_transaction_hooks(result, input) do
    input.after_transaction
    |> Enum.reduce(
      result,
      fn after_transaction, result ->
        tracer = input.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: input.domain,
            resource: input.resource,
            resource_short_name: Ash.Resource.Info.short_name(input.resource),
            actor: input.context[:private][:actor],
            tenant: input.context[:private][:tenant],
            action: input.action && input.action.name,
            authorize?: input.context[:private][:authorize?]
          }
        end

        Ash.Tracer.span :after_transaction,
                        "after_transaction",
                        tracer do
          Ash.Tracer.set_metadata(tracer, :after_transaction, metadata)

          Ash.Tracer.telemetry_span [:ash, :after_transaction], metadata do
            after_transaction.(input, result)
          end
        end
      end
    )
    |> case do
      :ok when has_no_return?(input) ->
        :ok

      {:ok, new_result} when has_return?(input) ->
        {:ok, new_result}

      {:error, error} ->
        {:error, error}

      other when has_return?(input) ->
        raise """
        Invalid return value from after_transaction hook. Because this action has a return type I expected one of:

        * {:ok, term}
        * {:error, error}

        Got:

        #{inspect(other)}
        """

      other when has_no_return?(input) ->
        raise """
        Invalid return value from after_transaction hook. Because this action has no return type I expected one of:

        * :ok
        * {:error, error}

        Got:

        #{inspect(other)}
        """
    end
  end

  @doc false
  def run_around_transaction_hooks(%{around_transaction: []} = input, func) do
    case func.(input) do
      :ok when has_no_return?(input) ->
        :ok

      {:ok, term} when has_return?(input) ->
        {:ok, term}

      {:error, error} ->
        {:error, error}

      other when has_no_return?(input) ->
        raise """
        Invalid return value from around_transaction hook. Because this action has no return type, I expected one of:

        * :ok
        * {:error, error}

        Got:

        #{inspect(other)}
        """

      other when has_return?(input) ->
        raise """
        Invalid return value from around_transaction hook. Because this action has a return type, I expected one of:

        * {:ok, term}
        * {:error, error}

        Got:

        #{inspect(other)}
        """
    end
  end

  def run_around_transaction_hooks(%{around_transaction: [around | rest]} = input, func) do
    around.(input, fn input ->
      run_around_transaction_hooks(%{input | around_transaction: rest}, func)
    end)
  end
end
