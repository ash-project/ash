defmodule Ash.Error.Invalid.NoSuchInput do
  @moduledoc "Used when an input is provided to an action that is not accepted"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :action, :input, :inputs], class: :invalid

  def splode_message(error) do
    """
    No such input `#{error.input}` for action #{inspect(error.resource)}.#{error.action}

    #{valid_inputs(error)}
    """
  end

  defp valid_inputs(error) do
    case Enum.filter(error.inputs, &is_atom/1) do
      [] ->
        "No valid inputs exist"

      inputs ->
        """

        Valid Inputs:

        #{Enum.map_join(inputs, "\n", &"* #{&1}")}
        """
    end
  end
end
