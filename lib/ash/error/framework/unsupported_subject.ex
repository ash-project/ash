defmodule Ash.Error.Framework.UnsupportedSubject do
  @moduledoc "Used when a subject is provided to a validation or preparation that it does not support"

  use Splode.Error, fields: [:subject, :module], class: :framework

  def message(error) do
    """
    Attempted to supply an #{inspect(error.subject)} to #{inspect(error.module)}, but it does not support that subject.
    """
  end
end
