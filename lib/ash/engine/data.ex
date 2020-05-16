defmodule Ash.Engine.Data do
  alias ETS.Set

  def init(requests) do
    {:ok, set} = Set.new(read_concurrency: true)

    init_requests(set, requests)

    set
  end

  defp init_requests(set, requests) do
    values = Enum.map(requests, &{&1.id, &1})

    case Set.put(set, values) do
      {:ok, _new_set} ->
        :ok

      other ->
        raise "Encountered an error replacing request: #{inspect(other)}"
    end
  end
end
