defmodule Ash.Error.Query.NoReadAction do
  @moduledoc "Used when a resource would be read but has no read action"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :when], class: :invalid

  def message(error) do
    if error.when do
      "No read action exists for  #{inspect(error.resource)} when: #{error.when}"
    else
      "No read action exists for  #{inspect(error.resource)}"
    end
  end
end
