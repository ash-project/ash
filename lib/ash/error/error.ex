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

  def to_error_class(value, opts) do
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
end
