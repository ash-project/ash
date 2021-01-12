defmodule Ash.Error do
  @moduledoc false
  @type error_class() :: :invalid | :authorization | :framework | :unknown

  # We use these error classes also to choose a single error
  # to raise when multiple errors have occured. We raise them
  # sorted by their error classes
  @error_classes [
    :forbidden,
    :invalid,
    :framework,
    :unknown
  ]

  alias Ash.Error.{Forbidden, Framework, Invalid, Unknown}

  @error_modules [
    forbidden: Forbidden,
    invalid: Invalid,
    framework: Framework,
    unknown: Unknown
  ]

  @error_class_indices @error_classes |> Enum.with_index() |> Enum.into(%{})

  defmodule Stacktrace do
    @moduledoc "A placeholder for a stacktrace so that we can avoid printing it everywhere"
    defstruct [:stacktrace]

    defimpl Inspect do
      def inspect(_, _) do
        "#Stacktrace<>"
      end
    end
  end

  def ash_error?(value) do
    !!impl_for(value)
  end

  def to_ash_error(values) when is_list(values) do
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

    choose_error(values)
  end

  def to_ash_error(value) do
    if ash_error?(value) do
      value
    else
      to_ash_error([value])
    end
  end

  @doc "A utility to flatten a list, but preserve keyword list elements"
  def flatten_preserving_keywords(list) do
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

  def clear_stacktraces(%{stacktrace: stacktrace} = error) when not is_nil(stacktrace) do
    clear_stacktraces(%{error | stacktrace: nil})
  end

  def clear_stacktraces(%{errors: errors} = exception) when is_list(errors) do
    %{exception | errors: Enum.map(errors, &clear_stacktraces/1)}
  end

  def clear_stacktraces(error), do: error

  def choose_error(errors) do
    [error | other_errors] =
      Enum.sort_by(errors, fn error ->
        # the second element here sorts errors that are already parent errors
        {Map.get(@error_class_indices, error.class),
         @error_modules[error.class] != error.__struct__}
      end)

    parent_error_module = @error_modules[error.class]

    if parent_error_module == error.__struct__ do
      %{error | errors: (error.errors || []) ++ other_errors}
    else
      parent_error_module.exception(errors: errors)
    end
  end

  def error_messages(errors, custom_message \\ nil, stacktraces? \\ false) do
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
              %{stacktrace: %Stacktrace{stacktrace: stacktrace}} = class_error ->
                "* #{Exception.message(class_error)}\n" <>
                  Enum.map_join(stacktrace, "\n", fn stack_item ->
                    "  " <> Exception.format_stacktrace_entry(stack_item)
                  end)
            end)
        else
          header <>
            Enum.map_join(class_errors, "\n", fn class_error ->
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
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Ash.Error.message(&1)}")
    end)
  end

  defp header(:invalid), do: "Input Invalid"
  defp header(:forbidden), do: "Forbidden"
  defp header(:framework), do: "Framework Error"
  defp header(:unknown), do: "Unknown Error"

  defmacro __using__(_) do
    quote do
      import Ash.Error, only: [def_ash_error: 1, def_ash_error: 2]
    end
  end

  defmacro def_ash_error(fields, opts \\ []) do
    quote do
      defexception unquote(fields) ++
                     [vars: [], path: [], stacktrace: [], class: unquote(opts)[:class]]

      @impl Exception
      def message(%{message: message, vars: vars} = exception) do
        string = message || ""

        string =
          Enum.reduce(vars, string, fn {key, value}, acc ->
            if String.contains?(acc, "%{#{key}}") do
              String.replace(acc, "%{#{key}}", to_string(value))
            else
              acc
            end
          end)

        Ash.Error.message(%{exception | message: string})
      end

      def message(exception), do: Ash.Error.message(exception)

      def exception(opts) do
        case Process.info(self(), :current_stacktrace) do
          {:current_stacktrace, [_, _ | stacktrace]} ->
            super(
              Keyword.put_new(opts, :stacktrace, %Ash.Error.Stacktrace{stacktrace: stacktrace})
            )

          _ ->
            super(opts)
        end
      end
    end
  end

  defdelegate id(error), to: Ash.ErrorKind
  defdelegate code(error), to: Ash.ErrorKind
  defdelegate message(error), to: Ash.ErrorKind
  defdelegate impl_for(error), to: Ash.ErrorKind
end

defprotocol Ash.ErrorKind do
  @moduledoc false

  @spec id(t()) :: String.t()
  def id(error)

  @spec code(t()) :: String.t()
  def code(error)

  @spec message(t()) :: String.t()
  def message(error)
end
