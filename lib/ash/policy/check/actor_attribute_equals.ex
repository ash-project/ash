defmodule Ash.Policy.Check.ActorAttributeEquals do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "actor.#{opts[:attribute]} == #{inspect(opts[:value])}"
  end

  @impl true
  def match?(nil, _, _), do: false

  def match?(actor, _context, opts) do
    with {:ok, actor_value} <- Map.fetch(actor, opts[:attribute]),
         {:ok, desired_value} <- Map.fetch(opts, :value) do
      Comp.equal?(actor_value, desired_value)
    else
      false
    end
  end
end
