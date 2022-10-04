defmodule Ash.Error do
  @moduledoc """
  Tools and utilities used by Ash to manage and conform errors
  """

  alias Ash.Error.{Forbidden, Framework, Invalid, Unknown}

  @type error_class() :: :invalid | :authorization | :framework | :unknown

  @type t :: struct

  # We use these error classes also to choose a single error
  # to raise when multiple errors have occurred. We raise them
  # sorted by their error classes
  @error_classes [
    :forbidden,
    :invalid,
    :framework,
    :unknown
  ]

  @error_modules [
    forbidden: Forbidden,
    invalid: Invalid,
    framework: Framework,
    unknown: Unknown
  ]

  @error_class_indices @error_classes |> Enum.with_index() |> Enum.into(%{})

  @doc false
  def error_modules, do: Keyword.values(@error_modules)

  defmodule Stacktrace do
    @moduledoc "A placeholder for a stacktrace so that we can avoid printing it everywhere"
    defstruct [:stacktrace]

    defimpl Inspect do
      def inspect(_, _) do
        "#Stacktrace<>"
      end
    end
  end

  @doc false
  def set_path(errors, path) when is_list(errors) do
    Enum.map(errors, &set_path(&1, path))
  end

  def set_path(error, path) when is_map(error) do
    path = List.wrap(path)

    error =
      if Map.has_key?(error, :path) && is_list(error.path) do
        %{error | path: path ++ error.path}
      else
        error
      end

    error =
      if Map.has_key?(error, :changeset) && error.changeset do
        %{error | changeset: %{error.changeset | errors: set_path(error.changeset.errors, path)}}
      else
        error
      end

    if Map.has_key?(error, :errors) && is_list(error.errors) do
      %{error | errors: Enum.map(error.errors, &set_path(&1, path))}
    else
      error
    end
  end

  def set_path(error, _), do: error

  def ash_error?(value) do
    !!Ash.ErrorKind.impl_for(value)
  end

  def to_error_class(values, opts \\ [])

  def to_error_class(values, opts) when is_list(values) do
    values =
      values
      |> flatten_preserving_keywords()
      |> Enum.uniq_by(&clear_stacktraces/1)
      |> Enum.map(fn value ->
        if ash_error?(value) do
          value
        else
          Unknown.exception(errors: values)
        end
      end)
      |> Enum.uniq()

    choose_error(values, opts[:changeset] || opts[:query])
  end

  def to_error_class(value, opts) do
    if ash_error?(value) && value.__struct__ in Keyword.values(@error_modules) do
      add_changeset_or_query(value, [value], opts[:changeset] || opts[:query])
    else
      to_error_class([value], opts)
    end
  end

  def to_ash_error(list, stacktrace \\ nil)

  def to_ash_error(list, stacktrace) when is_list(list) do
    if Keyword.keyword?(list) do
      error =
        list
        |> Keyword.take([:error, :vars])
        |> Keyword.put_new(:error, list[:message])
        |> Unknown.exception()

      if stacktrace do
        %{error | stacktrace: stacktrace}
      else
        stacktrace
      end
    else
      Enum.map(list, &to_ash_error(&1, stacktrace))
    end
  end

  def to_ash_error(error, stacktrace) when is_binary(error) do
    if stacktrace do
      error = Unknown.exception(error: error)
      %{error | stacktrace: %Stacktrace{stacktrace: stacktrace}}
    else
      Unknown.exception(error: error)
    end
  end

  def to_ash_error(other, stacktrace) do
    cond do
      ash_error?(other) ->
        if stacktrace do
          Map.put(other, :stacktrace, %Stacktrace{stacktrace: stacktrace})
        else
          other
        end

      is_exception(other) ->
        if stacktrace do
          other = Unknown.exception(error: Exception.format(:error, other))
          Map.put(other, :stacktrace, %Stacktrace{stacktrace: stacktrace})
        else
          Unknown.exception(error: Exception.format(:error, other))
        end

      true ->
        other = Unknown.exception(error: "unknown error: #{inspect(other)}")

        if stacktrace do
          Map.put(other, :stacktrace, %Stacktrace{stacktrace: stacktrace})
        else
          other
        end
    end
  end

  @doc "A utility to flatten a list, but preserve keyword list elements"
  def flatten_preserving_keywords(list) do
    if Keyword.keyword?(list) do
      [list]
    else
      Enum.flat_map(list, fn item ->
        cond do
          Keyword.keyword?(item) ->
            [item]

          is_list(item) ->
            item

          true ->
            [item]
        end
      end)
    end
  end

  def clear_stacktraces(%{stacktrace: stacktrace} = error) when not is_nil(stacktrace) do
    clear_stacktraces(%{error | stacktrace: nil})
  end

  def clear_stacktraces(%{errors: errors} = exception) when is_list(errors) do
    %{exception | errors: Enum.map(errors, &clear_stacktraces/1)}
  end

  def clear_stacktraces(error), do: error

  def choose_error(errors, changeset_or_query \\ nil)

  def choose_error([], changeset_or_query) do
    error = Ash.Error.Unknown.exception([])

    add_changeset_or_query(error, [], changeset_or_query)
  end

  def choose_error(errors, changeset_or_query) do
    errors = Enum.map(errors, &to_ash_error/1)

    [error | other_errors] =
      Enum.sort_by(errors, fn error ->
        # the second element here sorts errors that are already parent errors
        {Map.get(@error_class_indices, error.class),
         @error_modules[error.class] != error.__struct__}
      end)

    parent_error_module = @error_modules[error.class]

    top_level_error =
      if parent_error_module == error.__struct__ do
        %{error | errors: (error.errors || []) ++ other_errors}
      else
        parent_error_module.exception(errors: errors)
      end

    add_changeset_or_query(top_level_error, errors, changeset_or_query)
  end

  defp add_changeset_or_query(error, errors, changeset_or_query) do
    changeset = error.changeset || error.query || changeset_or_query

    if changeset_or_query do
      changeset_or_query = %{
        changeset_or_query
        | action_failed?: true,
          errors: List.wrap(errors) ++ changeset.errors
      }

      case changeset_or_query do
        %Ash.Changeset{} = changeset ->
          %{error | changeset: %{changeset | errors: Enum.uniq(changeset.errors)}}

        %Ash.Query{} = query ->
          %{error | query: %{query | errors: Enum.uniq(query.errors)}}
      end
    else
      error
    end
  end

  def error_messages(errors, custom_message, stacktraces?) do
    errors = Enum.map(errors, &to_ash_error/1)

    generic_message =
      errors
      |> List.wrap()
      |> Enum.group_by(& &1.class)
      |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
      |> Enum.map_join("\n\n", fn {class, class_errors} ->
        header = header(class) <> "\n\n"

        if stacktraces? do
          header <>
            Enum.map_join(class_errors, "\n", fn
              error when is_binary(error) ->
                "* #{error}"

              %{stacktrace: %Stacktrace{stacktrace: stacktrace}} = class_error ->
                "* #{Exception.message(class_error)}\n" <>
                  path(class_error) <>
                  Enum.map_join(stacktrace, "\n", fn stack_item ->
                    "  " <> Exception.format_stacktrace_entry(stack_item)
                  end)
            end)
        else
          header <>
            Enum.map_join(class_errors, "\n", fn
              class_error when is_binary(class_error) ->
                "* #{class_error}"

              class_error ->
                "* #{Exception.message(class_error)}"
            end)
        end
      end)

    if custom_message do
      custom =
        custom_message
        |> List.wrap()
        |> Enum.map_join("\n", &"* #{&1}")

      "\n\n" <> custom <> generic_message
    else
      generic_message
    end
  end

  def error_descriptions(errors) do
    errors
    |> Kernel.||([])
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Ash.ErrorKind.message(&1)}")
    end)
  end

  defp path(%{path: path}) when path not in [[], nil] do
    "    at " <> to_path(path) <> "\n"
  end

  defp path(_), do: ""

  defp to_path(path) do
    Enum.map_join(path, ", ", fn item ->
      if is_list(item) do
        "[#{to_path(item)}]"
      else
        if is_binary(item) || is_atom(item) || is_number(item) do
          item
        else
          inspect(item)
        end
      end
    end)
  end

  defp header(:invalid), do: "Input Invalid"
  defp header(:forbidden), do: "Forbidden"
  defp header(:framework), do: "Framework Error"
  defp header(:unknown), do: "Unknown Error"
end
