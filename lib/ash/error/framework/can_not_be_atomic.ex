defmodule Ash.Error.Framework.CanNotBeAtomic do
  @moduledoc "Used when a change that is only atomic cannot be done atomically"
  use Splode.Error, fields: [:resource, :change, :reason], class: :framework

  def message(error) do
    """
    #{inspect(error.resource)} #{inspect(error.change)} only has an atomic/3 callback, but cannot be performed atomically

    Reason: #{error.reason}
    """
  end
end
