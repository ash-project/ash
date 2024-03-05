defmodule Ash.Policy.Check.ActorAttributeEquals do
  @moduledoc "This check is true when the value of the specified attribute of the actor equals the specified value."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "actor.#{opts[:attribute]} == #{inspect(opts[:value])}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(nil, _, _), do: false

  def match?(actor, _context, opts) do
    with {:ok, actor_value} <- Map.fetch(actor, opts[:attribute]),
         {:ok, desired_value} <- Keyword.fetch(opts, :value) do
      Comp.equal?(actor_value, desired_value)
    else
      _ ->
        false
    end
  end
end
