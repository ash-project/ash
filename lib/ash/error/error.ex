defmodule Ash.Error do
  @moduledoc """
  Tools and utilities used by Ash to manage and conform errors
  """
  use Splode,
    error_classes: [
      forbidden: Ash.Error.Forbidden,
      invalid: Ash.Error.Invalid,
      framework: Ash.Error.Framework,
      unknown: Ash.Error.Unknown
    ],
    unknown_error: Ash.Error.Unknown.UnknownError

  @type ash_errors ::
          Ash.Error.Forbidden.t()
          | Ash.Error.Invalid.t()
          | Ash.Error.Framework.t()
          | Ash.Error.Unknown.t()
          | Ash.Error.Unknown.UnknownError.t()

  @type ash_error_fields :: Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t()
  @doc """
  Converts a value with optional stacktrace and opts to
  an error within Ash.
  """
  @spec to_ash_error(
          ash_error_fields() | term() | [ash_error_fields() | term()],
          list() | nil,
          Keyword.t()
        ) :: ash_errors() | [ash_errors()]
  def to_ash_error(value, stacktrace \\ nil, opts \\ []) do
    value =
      value
      |> List.wrap()
      |> Enum.map(fn
        %struct{} = changeset
        when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] ->
          to_error_class(changeset, opts)

        other ->
          other
      end)

    to_error(value, Keyword.put(opts, :stacktrace, stacktrace))
  end

  @doc """
  Converts a value to an Ash.Error type.
  """
  # @spec to_error_class(ash_error_fields() | term() | [ash_error_fields()] | [term()], Keyword.t()) :: ash_errors() | [ash_errors()]
  # having a hard time to figure out the return type
  def to_error_class(value, opts \\ [])

  def to_error_class(%Ash.Changeset{errors: errors} = changeset, opts) do
    to_error_class(errors, Keyword.put(opts, :changeset, %{changeset | errors: []}))
  end

  def to_error_class(%Ash.Query{errors: errors} = query, opts) do
    to_error_class(errors, Keyword.put(opts, :query, %{query | errors: []}))
  end

  def to_error_class(%Ash.ActionInput{errors: errors} = action_input, opts) do
    to_error_class(errors, Keyword.put(opts, :action_input, %{action_input | errors: []}))
  end

  def to_error_class([item], opts) do
    to_error_class(item, opts)
  end

  def to_error_class(value, opts) do
    value =
      value
      |> List.wrap()
      |> Enum.map(fn
        %Ash.Changeset{} = changeset ->
          to_error_class(changeset, opts)

        %Ash.Query{} = query ->
          to_error_class(query, opts)

        %Ash.ActionInput{} = action_input ->
          to_error_class(action_input, opts)

        other ->
          other
      end)

    class = to_class(value, opts)

    class =
      if changeset = opts[:changeset] do
        %{class | changeset: %{changeset | errors: class.errors}}
      else
        class
      end

    class =
      if action_input = opts[:action_input] do
        %{class | action_input: %{action_input | errors: class.errors}}
      else
        class
      end

    if query = opts[:query] do
      %{class | query: %{query | errors: class.errors}}
    else
      class
    end
  end

  @doc """
  Converts errors into a single `String.t`.
  """
  @spec error_descriptions(term() | [term()]) :: String.t()
  def error_descriptions(errors) do
    errors
    |> to_error_class()
    |> Map.get(:errors)
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} ->
      Enum.find_index([:forbidden, :invalid, :framework, :unknown], &(&1 == group))
    end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Exception.message(&1)}")
    end)
  end

  defp header(:forbidden), do: "Forbidden"
  defp header(:invalid), do: "Input Invalid"
  defp header(:framework), do: "Framework Error"
  defp header(:unknown), do: "Unknown Error"

  @doc """
  Returns whether or not a term is an Ash.Error type.
  """
  @spec ash_error?(term()) :: boolean()
  def ash_error?(value), do: splode_error?(value, __MODULE__)

  @doc """
  Sets the main path of the errors on the containing struct.
  """
  @spec set_path(ash_error_fields() | term(), term() | [term()]) ::
          ash_error_fields() | ash_errors()
  def set_path(%struct{errors: errors} = container, path)
      when struct in [Ash.Changeset, Ash.ActionInput, Ash.Query] do
    %{container | errors: set_path(errors, path)}
  end

  def set_path(error, path) do
    super(error, path)
  end
end
