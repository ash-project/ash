# SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Splode.ErrorClass do
  @moduledoc "Tools for working with error classes"

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      opts =
        Keyword.update(opts, :fields, [errors: []], fn fields ->
          has_error_fields? =
            Enum.any?(fields, fn
              :errors ->
                true

              {:errors, _} ->
                true

              _ ->
                false
            end)

          if has_error_fields? do
            fields
          else
            fields ++ [errors: []]
          end
          |> Keyword.put(:error_class?, true)
        end)
        |> Keyword.put(:error_class?, true)

      use Splode.Error, opts

      def message(%{errors: errors}) do
        Splode.ErrorClass.error_messages(errors)
      end
    end
  end

  @doc "Creates a long form composite error message for a list of errors"
  def error_messages(errors, opts \\ []) do
    custom_message = opts[:custom_message]
    errors = List.wrap(errors)

    {bread_crumbs, errors} =
      extract_shared_bread_crumbs(errors)

    generic_message =
      errors
      |> Enum.group_by(& &1.class)
      |> Enum.map_join("\n\n", fn {class, class_errors} ->
        header = String.capitalize(to_string(class)) <> " Error\n\n"

        header <>
          Enum.map_join(class_errors, "\n", fn
            error when is_binary(error) ->
              "* #{error}"

            %{stacktrace: %Splode.Stacktrace{stacktrace: stacktrace}} = class_error ->
              bread_crumb(class_error.bread_crumbs) <>
                "* #{Exception.message(class_error)}\n" <>
                path(class_error) <>
                Enum.map_join(stacktrace, "\n", fn stack_item ->
                  "  " <> Exception.format_stacktrace_entry(stack_item)
                end)

            class_error ->
              "* #{Exception.message(class_error)}\n" <>
                path(class_error)
          end)
      end)

    if custom_message do
      custom =
        custom_message
        |> List.wrap()
        |> Enum.map_join("\n", &"* #{&1}")

      "\n#{bread_crumb(bread_crumbs)}\n" <> custom <> generic_message
    else
      "\n#{bread_crumb(bread_crumbs)}" <>
        generic_message
    end
  end

  defp extract_shared_bread_crumbs(errors, shared \\ [])

  defp extract_shared_bread_crumbs(
         [%{bread_crumbs: [first | _rest_crumbs]} | rest] = errors,
         shared
       ) do
    if Enum.any?(rest, fn other_error ->
         !is_map(other_error) || !Map.has_key?(other_error, :bread_crumbs) ||
           Enum.at(other_error.bread_crumbs, 0) != first
       end) do
      {Enum.reverse(shared), errors}
    else
      extract_shared_bread_crumbs(
        Enum.map(errors, &%{&1 | bread_crumbs: Enum.drop(&1.bread_crumbs, 1)}),
        [first | shared]
      )
    end
  end

  defp extract_shared_bread_crumbs(errors, bread_crumbs), do: {Enum.reverse(bread_crumbs), errors}

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

  @doc false
  def bread_crumb(nil), do: ""
  def bread_crumb([]), do: ""

  def bread_crumb(bread_crumbs) do
    case Enum.filter(bread_crumbs, & &1) do
      [] ->
        ""

      bread_crumbs ->
        "Bread Crumbs:\n" <> join_bread_crumbs(bread_crumbs) <> "\n"
    end
  end

  defp join_bread_crumbs(bread_crumbs) do
    Enum.reduce(bread_crumbs, {"", []}, fn bread_crumb, {line, lines} ->
      case String.length(line) + String.length(bread_crumb) do
        length when length < 80 ->
          {line <> "> " <> bread_crumb, lines}

        _ ->
          {"> " <> bread_crumb, ["  " <> line | lines]}
      end
    end)
    |> then(fn {line, lines} ->
      lines =
        case line do
          "" ->
            lines

          line ->
            ["  " <> line | lines]
        end

      lines
      |> Enum.reject(&(&1 == ""))
      |> Enum.join("\n")
      |> Kernel.<>("\n")
    end)
  end
end
