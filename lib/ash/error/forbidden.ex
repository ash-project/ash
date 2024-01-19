defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"

  use Ash.Error.Exception

  def_ash_error([:errors], class: :forbidden)

  @type t :: %__MODULE__{}

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def message(%{errors: errors, stacktrace: %{stacktrace: stacktrace}} = error)
        when is_nil(errors) or errors == [] do
      Ash.Error.breadcrumb(error.error_context) <>
        "* Forbidden\n" <>
        Enum.map_join(stacktrace || "", "\n", fn stack_item ->
          "  " <> Exception.format_stacktrace_entry(stack_item)
        end)
    end

    def message(%{errors: errors}) do
      Ash.Error.error_messages(errors || [], nil)
    end

    def code(_), do: "forbidden"
  end
end
