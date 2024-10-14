defmodule Ash.ActionInput do
  @moduledoc """
  Input for a custom action
  """

  alias Ash.Error.Action.InvalidArgument

  require Ash.Flags

  defstruct [
    :action,
    :domain,
    :resource,
    :tenant,
    invalid_keys: MapSet.new(),
    arguments: %{},
    params: %{},
    context: %{},
    valid?: true,
    errors: []
  ]

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
    actor: [
      type: :any,
      doc: "The actor performing the action"
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      doc: "A list of unknow inputs to skip. Use `:*` to skip all unknown inputs."
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
  Creates a new input for a generic action

  ## Options

  #{Opts.docs()}
  """
  @doc spark_opts: [{4, @for_action_opts}]
  @spec for_action(
          resource_or_input :: Ash.Resource.t() | t(),
          action :: atom,
          params :: map,
          opts :: Keyword.t()
        ) :: t()
  def for_action(resource_or_input, action, params, opts \\ []) do
    case Opts.validate(opts) do
      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}

      {:ok, opts} ->
        opts = Opts.to_options(opts)

        input =
          case resource_or_input do
            resource when is_atom(resource) ->
              action = Ash.Resource.Info.action(resource, action)
              %__MODULE__{resource: resource, action: action}

            input ->
              action = Ash.Resource.Info.action(input.resource, action)
              %{input | action: action}
          end

        domain =
          input.domain || opts[:domain] || Ash.Resource.Info.domain(input.resource) ||
            Ash.Actions.Helpers.maybe_embedded_domain(input.resource) ||
            raise ArgumentError,
              message:
                "Could not determine domain for action. Provide the `domain` option or configure a domain in the resource directly."

        input = %{input | domain: domain}

        {input, _opts} = Ash.Actions.Helpers.set_context_and_get_opts(input.domain, input, opts)

        input =
          Enum.reduce(opts[:private_arguments] || %{}, input, fn {k, v}, input ->
            Ash.ActionInput.set_argument(input, k, v)
          end)

        input
        |> cast_params(params, opts)
        |> set_defaults()
        |> require_arguments()
    end
  end

  @spec set_tenant(t(), Ash.ToTenant.t()) :: t()
  def set_tenant(input, tenant) do
    %{input | tenant: tenant}
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

  @doc "Gets the value of an argument provided to the input."
  @spec get_argument(t, atom | String.t()) :: term
  def get_argument(input, argument) when is_atom(argument) or is_binary(argument) do
    case fetch_argument(input, argument) do
      {:ok, value} -> value
      :error -> nil
    end
  end

  @doc "Fetches the value of an argument provided to the input or `:error`."
  @spec fetch_argument(t, atom | String.t()) :: {:ok, term()} | :error
  def fetch_argument(input, argument) when is_atom(argument) or is_binary(argument) do
    with :error <- Map.fetch(input.arguments, argument) do
      Map.fetch(input.arguments, to_string(argument))
    end
  end

  @doc "Set an argument value"
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
  Deep merges the provided map into the input context that can be used later

  Do not use the `private` key in your custom context, as that is reserved for internal use.
  """
  @spec set_context(t(), map | nil) :: t()
  def set_context(input, nil), do: input

  def set_context(input, map) do
    %{input | context: Ash.Helpers.deep_merge_maps(input.context, map)}
  end

  defp cast_params(input, params, opts) do
    input = %{
      input
      | params: Map.merge(input.params, Enum.into(params, %{}))
    }

    skip_unknown_inputs = opts[:skip_unknown_inputs] || []

    Enum.reduce(params, input, fn {name, value}, input ->
      cond do
        has_argument?(input.action, name) ->
          set_argument(input, name, value)

        match?("_" <> _, name) ->
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

  @doc "Adds an error to the input errors list, and marks the input as `valid?: false`"
  @spec add_error(t(), term | String.t() | list(term | String.t())) :: t()
  def add_error(input, errors, path \\ [])

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
