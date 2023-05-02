defmodule Ash.ActionInput do
  @moduledoc """
  Input for a custom action
  """

  alias Ash.Error.Action.InvalidArgument

  defstruct [
    :action,
    :api,
    :resource,
    arguments: %{},
    params: %{},
    context: %{},
    valid?: true,
    errors: []
  ]

  @type t :: %__MODULE__{
          arguments: map(),
          params: map(),
          action: Ash.Resource.Actions.Action.t(),
          resource: Ash.Resource.t(),
          context: map(),
          api: Ash.Api.t(),
          valid?: boolean()
        }

  @doc """
  Creates a new input for a basic action
  """
  @spec for_action(
          resource_or_input :: Ash.Resource.t() | t(),
          action :: atom,
          params :: map,
          opts :: Keyword.t()
        ) :: t()
  def for_action(resource_or_input, action, params, opts \\ []) do
    input =
      case resource_or_input do
        resource when is_atom(resource) ->
          action = Ash.Resource.Info.action(resource, action)
          %__MODULE__{resource: resource, action: action}

        input ->
          input
      end

    {input, _opts} = Ash.Actions.Helpers.add_process_context(input.api, input, opts)

    cast_params(input, params)
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
        with {:ok, casted} <-
               Ash.Type.Helpers.cast_input(argument.type, value, argument.constraints, input),
             {:constrained, {:ok, casted}, argument} when not is_nil(casted) <-
               {:constrained,
                Ash.Type.apply_constraints(argument.type, casted, argument.constraints),
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
        %{input | arguments: Map.put(input.arguments, argument, value)}
      end
    else
      %{input | arguments: Map.put(input.arguments, argument, value)}
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

  defp cast_params(input, params) do
    input = %{
      input
      | params: Map.merge(input.params, Enum.into(params, %{}))
    }

    Enum.reduce(params, input, fn {name, value}, input ->
      if has_argument?(input.action, name) do
        set_argument(input, name, value)
      else
        input
      end
    end)
  end

  defp has_argument?(action, name) when is_atom(name) do
    Enum.any?(action.arguments, &(&1.private? == false && &1.name == name))
  end

  defp has_argument?(action, name) when is_binary(name) do
    Enum.any?(action.arguments, &(&1.private? == false && to_string(&1.name) == name))
  end

  defp add_invalid_errors(value, input, attribute, message) do
    messages =
      if Keyword.keyword?(message) do
        [message]
      else
        List.wrap(message)
      end

    Enum.reduce(messages, input, fn message, input ->
      if Exception.exception?(message) do
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
