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

  @error_modules [
    forbidden: Ash.Error.Forbidden,
    invalid: Ash.Error.Invalid,
    framework: Ash.Error.Framework,
    unknown: Ash.Error.Unknown
  ]

  @error_class_indices @error_classes |> Enum.with_index() |> Enum.into(%{})

  def choose_error(errors) do
    [error | _other_errors] = Enum.sort_by(errors, &Map.get(@error_class_indices, &1.class))

    parent_error_module = @error_modules[error.class]

    parent_error_module.exception(errors: errors)
  end

  def error_messages(errors) do
    errors
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Exception.message(&1)}")
    end)
  end

  def error_descriptions(errors) do
    errors
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Ash.Error.description(&1)}")
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
      defexception unquote(fields) ++ [:other_errors, path: [], class: unquote(opts)[:class]]

      @impl Exception
      defdelegate message(error), to: Ash.Error

      def exception(opts) do
        case opts[:stacktrace] do
          true ->
            case Process.info(self(), :current_stacktrace) do
              {:current_stacktrace, stacktrace} ->
                stacktrace = Enum.drop(stacktrace, 2)

                opts
                |> Keyword.put(:stacktrace, stacktrace)
                |> super()
            end

          _ ->
            super(opts)
        end
      end
    end
  end

  defdelegate id(error), to: Ash.ErrorKind
  defdelegate code(error), to: Ash.ErrorKind
  defdelegate message(error), to: Ash.ErrorKind
  defdelegate description(error), to: Ash.ErrorKind
  defdelegate impl_for(error), to: Ash.ErrorKind
end

defprotocol Ash.ErrorKind do
  @spec id(t()) :: String.t()
  def id(error)

  @spec code(t()) :: String.t()
  def code(error)

  @spec message(t()) :: String.t()
  def message(error)

  @spec description(t()) :: String.t()
  def description(error)
end
