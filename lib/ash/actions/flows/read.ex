defmodule Ash.Actions.Flows.Read do
  @moduledoc """
  Execute a read action.
  """
  require Ash.Flags
  use Ash.Flow

  flow do
    argument :query, :struct do
      allow_nil? false
      constraints instance_of: Ash.Query
    end

    argument :action, :struct do
      allow_nil? false
      constraints instance_of: Ash.Resource.Actions.Read
    end

    returns :fake_result
  end

  steps do
    custom :fake_result, Ash.Actions.Flows.Read.FakeResult do
      input %{query: arg(:query), action: arg(:action)}
    end
  end

  @dialyzer {:no_return, [run: 3]}
  def run(query, action, opts) do
    Ash.Flags.assert!(:read_uses_flow?, true)

    __MODULE__
    |> Ash.Flow.run(%{query: query, action: action}, opts)
    |> case do
      result when result.errors == [] ->
        {:ok, result.result}

      result ->
        {:error, result}
    end
  end
end
