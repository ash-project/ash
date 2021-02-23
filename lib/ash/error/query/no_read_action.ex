defmodule Ash.Error.Query.NoReadAction do
  @moduledoc "Used when a resource would be read but has no read action"
  use Ash.Error.Exception

  def_ash_error([:resource, :when], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_read_action"

    def message(error) do
      if error.when do
        "No read action exists for  #{inspect(error.resource)} when: #{error.when}"
      else
        "No read action exists for  #{inspect(error.resource)}"
      end
    end
  end
end
