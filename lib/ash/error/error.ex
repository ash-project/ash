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

  def ash_error?(value), do: splode_error?(value, __MODULE__)

  def set_path(%struct{errors: errors} = container, path)
      when struct in [Ash.Changeset, Ash.ActionInput, Ash.Query] do
    %{container | errors: set_path(errors, path)}
  end

  def set_path(error, path) do
    super(error, path)
  end
end
