# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  @type error_keyword_option ::
          {:field, atom()}
          | {:fields, list(atom)}
          | {:value, term()}
          | {:message, String.t()}
          | {:path, list(atom | String.t())}

  @type error_keyword :: list(error_keyword_option)

  @type ash_error :: Exception.t()
  @type ash_error_subject :: Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t()
  @type path :: [String.t() | atom() | integer()]
  @type path_input :: [String.t() | atom() | integer()] | String.t() | atom() | integer()

  @type error_input ::
          ash_error() | error_keyword() | String.t() | ash_error_subject() | Exception.t() | any()

  @doc """
  Converts a value to an Ash exception.

  The supported inputs to this function can be provided to various places,
  like `Ash.Query.add_error/2`, `Ash.Changeset.add_error/2` and `Ash.ActionInput.add_error/2`.

  Additionally, any place that you can *return* an error you can return instead a valid
  error input.

  See [the error handling guide](/documentation/development/error-handling.md) for more.
  """
  @spec to_ash_error(
          error_input() | list(error_input),
          Exception.stacktrace() | nil,
          Keyword.t()
        ) :: ash_error() | [ash_error()]
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

  @spec to_error_class(
          ash_error_subject() | term() | [ash_error_subject()] | [term()],
          Keyword.t()
        ) ::
          t()
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
    |> remove_required_if_other_errors_exist()
  end

  defp remove_required_if_other_errors_exist(%{errors: errors} = class) do
    {required, errors} =
      Enum.split_with(errors, fn
        %required{field: field} = error
        when required in [Ash.Error.Query.Required, Ash.Error.Changes.Required] ->
          Enum.any?(errors, fn other_error ->
            other_error != error && for_field?(other_error, field, error.path)
          end)

        _other ->
          false
      end)

    errors =
      required
      |> Enum.group_by(fn error -> {error.path, error.field} end)
      |> Enum.reduce(errors, fn {{path, field}, required_errors}, errors ->
        if Enum.any?(errors, fn other_error ->
             for_field?(other_error, field, path)
           end) do
          errors
        else
          [Enum.max_by(required_errors, &Enum.count(&1.bread_crumbs))]
        end
      end)

    %{class | errors: errors}
  end

  defp remove_required_if_other_errors_exist(class), do: class

  defp for_field?(%{field: field, path: path}, field, path), do: true

  defp for_field?(%{fields: fields, path: path}, field, path) do
    field in List.wrap(fields)
  end

  defp for_field?(_, _, _), do: false

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
  This function prepends the provided path to any existing path on the errors.
  """
  @spec set_path(ash_error() | list(ash_error()), path_input()) :: ash_error() | list(ash_error())

  @spec set_path(ash_error_subject(), path_input()) :: ash_error_subject()
  def set_path(%struct{errors: errors} = container, path)
      when struct in [Ash.Changeset, Ash.ActionInput, Ash.Query] do
    %{container | errors: set_path(errors, path)}
  end

  def set_path(errors, path) when is_list(errors) do
    Enum.map(errors, &set_path(&1, path))
  end

  def set_path(error, path) do
    super(error, path)
  end

  @doc false
  def override_validation_message(error, message) do
    case error do
      %{field: field} = error when not is_nil(field) ->
        error
        |> Map.take([:field, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
        |> Keyword.put(:value, Map.get(error, :value))
        |> Ash.Error.Changes.InvalidAttribute.exception()

      %{fields: fields} when fields not in [nil, []] ->
        error
        |> Map.take([:fields, :vars])
        |> Map.to_list()
        |> Keyword.put(:message, message)
        |> Keyword.put(:value, Map.get(error, :value))
        |> Ash.Error.Changes.InvalidChanges.exception()

      _ ->
        message
    end
  end
end
