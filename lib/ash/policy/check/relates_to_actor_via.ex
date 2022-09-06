defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc false
  use Ash.Policy.FilterCheck

  require Ash.Expr

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

    Ash.Expr.expr(exists(^opts[:relationship_path], ^Enum.map(pkey, &{&1, {:_actor, &1}})))
  end
end
