defmodule Ash.ActionInput do
  @moduledoc """
  Input for a custom action

  Much like an `Ash.Query` and `Ash.Changeset` are used to provide inputs into
  CRUD actions, this struct provides the inputs required to execute a generic
  action.
  """

  alias Ash.Error.Action.InvalidArgument

  require Ash.Flags

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
    errors: []
  ]

  @typedoc """
  An action input struct for generic (non-CRUD) actions.

  Contains all the information needed to execute a generic action including
  arguments, context, tenant information, and validation state. Built using
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
          domain: Ash.Domain.t(),
          valid?: boolean()
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
  @spec new(Ash.Resource.t(), Ash.Domain.t()) :: t
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
  @spec set_argument(input :: t(), name :: atom, value :: term()) :: t()
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
end
