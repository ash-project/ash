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

    #{hint(error)}
    #{valid_inputs(error)}
    """
  end

  defp hint(error) do
    action = Ash.Resource.Info.action(error.resource, error.action)

    if action do
      case Ash.Resource.Info.attribute(error.resource, error.input) do
        nil ->
          "No such attribute on #{inspect(error.resource)}, or argument on #{inspect(error.resource)}.#{error.action}"

        %{writable?: false} ->
          """
          The attribute exists on #{inspect(error.resource)}, but is not accepted by #{inspect(error.resource)}.#{error.action}

          The attribute is currently `writable?: false`, which means that it can never be accepted by an action.
          """

        %{public?: false} ->
          """
          The attribute exists on #{inspect(error.resource)}, but is not accepted by #{inspect(error.resource)}.#{error.action}

          The attribute is currently `public?: false`, which means that it is not accepted when using `:*`, i.e in `accept :*`.

          Perhaps you meant to make the attribute public, or add it to the accept list for #{inspect(error.resource)}.#{error.action}?
          """

        _attribute ->
          """
          The attribute exists on #{inspect(error.resource)}, but is not accepted by #{inspect(error.resource)}.#{error.action}

          Perhaps you meant to add it to the accept list for #{inspect(error.resource)}.#{error.action}?
          """
      end
    end
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
