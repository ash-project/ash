defmodule Ash.Error.Invalid.NoSuchInput do
  @moduledoc "Used when an input is provided to an action or calculation that is not accepted"
  use Ash.Error.Exception

  use Splode.Error,
    fields: [:calculation, :resource, :action, :input, :inputs, did_you_mean: []],
    class: :invalid

  def exception(opts) do
    string_input = to_string(opts[:input])

    did_you_mean =
      opts[:inputs]
      |> Enum.filter(&is_binary/1)
      |> Enum.map(fn potential_input ->
        {potential_input, String.jaro_distance(potential_input, string_input)}
      end)
      |> Enum.filter(fn {_, score} ->
        score >= 0.8
      end)
      |> Enum.sort_by(&elem(&1, 1))
      |> Enum.map(&elem(&1, 0))

    super(Keyword.put(opts, :did_you_mean, did_you_mean))
  end

  def message(%{calculation: calculation} = error) when not is_nil(calculation) do
    calculation =
      if is_atom(calculation) do
        calculation
      else
        inspect(calculation)
      end

    """
    No such input `#{error.input}` for calculation #{calculation}

    #{valid_inputs(error)}
    """
  end

  def message(error) do
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
        #{did_you_mean(error)}
        Valid Inputs:

        #{Enum.map_join(inputs, "\n", &"* #{&1}")}
        """
    end
  end

  defp did_you_mean(error) do
    case error.did_you_mean do
      [] ->
        ""

      potential_inputs ->
        bullets = Enum.map_join(potential_inputs, "\n", &"* #{&1}")

        """

        Did you mean:

        #{bullets}

        """
    end
  end
end
