defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc false
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")
    "record.#{path} == actor"
  end

  @impl true
  def filter(opts) do
    pkey =
      opts[:resource]
      |> Ash.Resource.Info.related(opts[:relationship_path])
      |> Kernel.||(raise "Must be able to determine related resource for `relates_to_actor_via`")
      |> Ash.Resource.Info.primary_key()

    put_in_path(opts[:relationship_path], Enum.map(pkey, &{&1, {:_actor, &1}}))
  end

  @impl true
  def reject(opts) do
    pkey =
      opts[:resource]
      |> Ash.Resource.Info.related(opts[:relationship_path])
      |> Kernel.||(raise "Must be able to determine related resource for `relates_to_actor_via`")
      |> Ash.Resource.Info.primary_key()

    [
      or: [
        [not: filter(opts)],
        [put_in_path(opts[:relationship_path], Enum.map(pkey, &{:is_nil, &1}))]
      ]
    ]
  end

  defp put_in_path([], value) do
    value
  end

  defp put_in_path([key | rest], value) do
    [{key, put_in_path(rest, value)}]
  end
end
